use crate::core::document::Document;
use crate::core::document::Language;
use crate::core::hex::ToHex as _;
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use strum::IntoEnumIterator as _;
use time::OffsetDateTime;
use tokio::sync::RwLock;

pub struct Context {
  pub schema: tantivy::schema::Schema,
  pub fields: FieldSet,
  pub index: tantivy::Index,
  pub index_reader: tantivy::IndexReader,
  pub updated_at: RwLock<Option<OffsetDateTime>>,
  pub current_at: RwLock<Option<OffsetDateTime>>,
}

#[derive(Clone)]
pub struct FieldSet {
  pub type_: tantivy::schema::Field,
  pub id: tantivy::schema::Field,
  pub created_at: tantivy::schema::Field,
  pub created_by: tantivy::schema::Field,
  pub updated_at: tantivy::schema::Field,
  pub subforum_id: tantivy::schema::Field,
  pub topic_id: tantivy::schema::Field,
  pub taxon_rank: tantivy::schema::Field,
  pub parents: tantivy::schema::Field,
  pub title: tantivy::schema::Field,
  pub names: tantivy::schema::Field,
  pub tags: tantivy::schema::Field,
  pub content: tantivy::schema::Field,
}

pub fn load_map() -> Arc<HashMap<Language, Arc<Context>>> {
  let result: Result<
    Arc<HashMap<Language, Arc<Context>>>,
    Box<dyn StdError + Send + Sync>,
  > = (|| {
    let (schema, fields) = build_schema();
    Ok(Arc::new(
      Language::iter()
        .map(|language| {
          let schema = schema.clone();
          let fields = fields.clone();
          let index = load_index(&schema, language)?;
          let index_reader = index
            .reader_builder()
            .reload_policy(tantivy::ReloadPolicy::OnCommit)
            .try_into()?;
          let (updated_at, current_at) =
            parse_index_meta(&index)?;
          let updated_at = RwLock::new(updated_at);
          let current_at = RwLock::new(current_at);
          Ok((
            language,
            Arc::new(Context {
              schema,
              fields,
              index,
              index_reader,
              updated_at,
              current_at,
            }),
          ))
        })
        .collect::<Result<
          HashMap<Language, Arc<Context>>,
          Box<dyn StdError + Send + Sync>,
        >>()?,
    ))
  })();
  result.expect("search load error")
}

fn build_schema() -> (tantivy::schema::Schema, FieldSet) {
  let mut schema_builder =
    tantivy::schema::Schema::builder();
  let fields = FieldSet {
    type_: schema_builder
      .add_facet_field("type", get_facet_options()),
    id: schema_builder
      .add_text_field("id", get_string_options()),
    created_at: schema_builder
      .add_i64_field("created_at", get_int_options()),
    created_by: schema_builder
      .add_text_field("created_by", get_string_options()),
    updated_at: schema_builder
      .add_i64_field("updated_at", get_int_options()),
    subforum_id: schema_builder
      .add_facet_field("subforum_id", get_facet_options()),
    topic_id: schema_builder
      .add_text_field("topic_id", get_string_options()),
    taxon_rank: schema_builder
      .add_facet_field("taxon_rank", get_facet_options()),
    parents: schema_builder
      .add_facet_field("parents", get_facet_options()),
    title: schema_builder
      .add_text_field("title", get_text_options()),
    names: schema_builder
      .add_text_field("names", get_text_options()),
    tags: schema_builder
      .add_facet_field("tags", get_facet_options()),
    content: schema_builder
      .add_text_field("content", get_text_options()),
  };
  let schema = schema_builder.build();
  (schema, fields)
}

fn get_string_options() -> tantivy::schema::TextOptions {
  tantivy::schema::STRING.set_stored()
}

fn get_text_options() -> tantivy::schema::TextOptions {
  tantivy::schema::TextOptions::default()
      .set_indexing_options(
        tantivy::schema::TextFieldIndexing::default()
          .set_tokenizer("basic")
          .set_index_option(
            tantivy::schema::IndexRecordOption::WithFreqsAndPositions,
          )
      )
      .set_stored()
}

fn get_int_options() -> tantivy::schema::IntOptions {
  tantivy::schema::IntOptions::default()
    .set_indexed()
    .set_stored()
}

fn get_facet_options() -> tantivy::schema::FacetOptions {
  tantivy::schema::FacetOptions::default()
    .set_indexed()
    .set_stored()
}

pub fn load_index(
  schema: &tantivy::schema::Schema,
  language: Language,
) -> Result<tantivy::Index, Box<dyn StdError + Send + Sync>>
{
  let index_path =
    format!("./.tantivy/document_{}", language.as_ref());
  let index_path = Path::new(index_path.as_str());
  println!("Loading index {:?}", index_path);
  fs::create_dir_all(index_path)?;
  let index_dir =
    tantivy::directory::MmapDirectory::open(index_path)?;
  let index = tantivy::Index::open_or_create(
    index_dir,
    schema.clone(),
  )?;
  let tokenizer = match language {
    Language::Japanese =>
      tantivy::tokenizer::TextAnalyzer::from(
        lindera_tantivy::tokenizer::LinderaTokenizer::with_config(lindera::tokenizer::TokenizerConfig {
          dict_path: None,
          user_dict_path: None, user_dict_bin_path: None,
          mode: lindera_core::viterbi::Mode::Decompose(lindera_core::viterbi::Penalty::default()),
        })?
      )
      .filter(tantivy::tokenizer::LowerCaser),
    _ =>
      tantivy::tokenizer::TextAnalyzer::from(
        tantivy::tokenizer::SimpleTokenizer,
      )
      .filter(tantivy::tokenizer::LowerCaser)
      .filter(tantivy::tokenizer::Stemmer::new(
        match language {
          Language::English => tantivy::tokenizer::Language::English,
          _ => tantivy::tokenizer::Language::French,
        }
      ))
  };
  index.tokenizers().register("basic", tokenizer);
  Ok(index)
}

fn parse_index_meta(
  index: &tantivy::Index,
) -> Result<
  (Option<OffsetDateTime>, Option<OffsetDateTime>),
  Box<dyn StdError + Send + Sync>,
> {
  let index_meta = index.load_metas()?;
  if let Some(payload) = index_meta.payload {
    if let [updated_at, current_at] = &payload
      .split('/')
      .map(|item| {
        Ok(OffsetDateTime::from_unix_timestamp(
          item.parse()?,
        )?)
      })
      .collect::<Result<
        Vec<OffsetDateTime>,
        Box<dyn StdError + Send + Sync>,
      >>()?[..]
    {
      Ok((Some(*updated_at), Some(*current_at)))
    } else {
      Err("invalid index meta".into())
    }
  } else {
    Ok((None, None))
  }
}

pub fn update(
  search_context: Arc<Context>,
  new_updated_at: OffsetDateTime,
  new_current_at: OffsetDateTime,
  documents: Vec<Document>,
) {
  let result: Result<(), Box<dyn StdError + Send + Sync>> =
    (|| {
      let mut index_writer =
        search_context.index.writer(50_000_000)?;
      for document in documents {
        update_document(
          &search_context,
          &index_writer,
          document,
        )?;
      }
      let mut prepared_commit =
        index_writer.prepare_commit()?;
      prepared_commit.set_payload(
        &[new_updated_at, new_current_at]
          .map(|item| item.unix_timestamp().to_string())
          .join("/"),
      );
      prepared_commit.commit()?;
      Ok(())
    })();
  result.expect("search update error");
}

pub fn update_document(
  search_context: &Arc<Context>,
  index_writer: &tantivy::IndexWriter,
  document: Document,
) -> Result<(), Box<dyn StdError + Send + Sync>> {
  let fields = &search_context.fields;
  let id = document.id.to_hex();
  index_writer.delete_term(tantivy::Term::from_field_text(
    fields.id, &id,
  ));
  let mut search_document =
    tantivy::schema::Document::default();
  search_document
    .add_facet_path(fields.type_, &[document.type_]);
  search_document.add_text(fields.id, &id);
  search_document.add_i64(
    fields.created_at,
    document.created_at.unix_timestamp(),
  );
  search_document.add_text(
    fields.created_by,
    &document.created_by.to_hex(),
  );
  search_document.add_i64(
    fields.updated_at,
    document.updated_at.unix_timestamp(),
  );
  if let Some(subforum_id) = document.subforum_id {
    search_document.add_facet_path(
      fields.subforum_id,
      &[&subforum_id.to_hex()],
    );
  }
  if let Some(topic_id) = document.topic_id {
    search_document
      .add_text(fields.topic_id, &topic_id.to_hex());
  }
  if let Some(taxon_rank) = document.taxon_rank {
    search_document.add_facet_path(
      fields.taxon_rank,
      &[taxon_rank.as_ref()],
    );
  }
  if let Some(parents) = document.parents {
    search_document.add_facet_path(
      fields.taxon_rank,
      parents.iter().map(|parent| parent.to_hex()),
    );
  }
  if let Some(title) = document.title {
    search_document.add_text(fields.title, &title);
  }
  for name in document.names.unwrap_or_default() {
    search_document.add_text(fields.names, &name);
  }
  for tag in document.tags.unwrap_or_default() {
    search_document
      .add_facet_path(fields.names, &[&tag.to_hex()]);
  }
  search_document
    .add_text(fields.content, &document.content);
  index_writer.add_document(search_document);
  Ok(())
}

trait IndexWriterExt {
  fn add_facet_path<Path>(
    &mut self,
    field: tantivy::schema::Field,
    path: Path,
  ) where
    Path: IntoIterator,
    Path::Item: ToString;
}

impl IndexWriterExt for tantivy::Document {
  fn add_facet_path<Path>(
    &mut self,
    field: tantivy::schema::Field,
    path: Path,
  ) where
    Path: IntoIterator,
    Path::Item: ToString,
  {
    self.add_facet(
      field,
      tantivy::schema::Facet::from_path(path),
    );
  }
}
