use crate::core::document::Language;
use std::collections::HashMap;
use std::error::Error as StdError;
use std::fs;
use std::path::Path;
use strum::IntoEnumIterator as _;

pub struct Context {
  pub schema: tantivy::schema::Schema,
  pub fields: FieldSet,
  pub instances: HashMap<Language, Instance>,
}

pub struct Instance {
  pub index: tantivy::Index,
  pub index_reader: tantivy::IndexReader,
}

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
  pub name: tantivy::schema::Field,
  pub content: tantivy::schema::Field,
}

pub fn load(
) -> Result<Context, Box<dyn StdError + Send + Sync>> {
  let (schema, fields) = build_schema();
  let instances = Language::iter()
    .map(|language| {
      load_index(&schema, language)
        .map(|instance| (language, instance))
    })
    .collect::<Result<
      HashMap<Language, Instance>,
      Box<dyn StdError + Send + Sync>,
    >>()?;
  Ok(Context {
    schema,
    fields,
    instances,
  })
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
    name: schema_builder
      .add_text_field("name", get_text_options()),
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
) -> Result<Instance, Box<dyn StdError + Send + Sync>> {
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
  let index_reader = index
    .reader_builder()
    .reload_policy(tantivy::ReloadPolicy::OnCommit)
    .try_into()?;
  Ok(Instance {
    index,
    index_reader,
  })
}

pub fn _update(
  search_context: &Context,
) -> Result<(), Box<dyn StdError + Send + Sync>> {
  let instance = search_context
    .instances
    .get(&Language::Japanese)
    .ok_or("Invalid language")?;
  let fields = &search_context.fields;
  let index_meta = instance.index.load_metas()?;
  if index_meta.payload != Some("TEST".to_string()) {
    let mut index_writer =
      instance.index.writer(50_000_000)?;
    index_writer.delete_term(
      tantivy::Term::from_field_text(fields.id, "a"),
    );
    let mut old_man_doc =
      tantivy::schema::Document::default();
    old_man_doc.add_text(fields.id, "a");
    old_man_doc
      .add_text(fields.title, "The Old Man and the Sea");
    old_man_doc.add_text(
        fields.content,
        "He was an old man who fished alone in a skiff in the Gulf Stream and \
         he had gone eighty-four days now without taking a fish.",
    );

    // ... and add it to the `IndexWriter`.
    index_writer.add_document(old_man_doc);
    index_writer.add_document(tantivy::doc!(
      fields.title => "Of Mice and Men",
      fields.content => "A few miles south of Soledad, the Salinas River drops in close to the hillside \
              bank and runs deep and green. The water is warm too, for it has slipped twinkling \
              over the yellow sands in the sunlight before reaching the narrow pool. On one \
              side of the river the golden foothill slopes curve up to the strong and rocky \
              Gabilan Mountains, but on the valley side the water is lined with trees—willows \
              fresh and green with every spring, carrying in their lower leaf junctures the \
              debris of the winter’s flooding; and sycamores with mottled, white, recumbent \
              limbs and branches that arch over the pool"
    ));
    index_writer.add_document(tantivy::doc!(
      fields.title => "Frankenstein",
      fields.title => "The Modern Prometheus",
      fields.content => "You will rejoice to hear that no disaster has accompanied the commencement of an \
               enterprise which you have regarded with such evil forebodings.  I arrived here \
               yesterday, and my first task is to assure my dear sister of my welfare and \
               increasing confidence in the success of my undertaking."
      ));
    let mut prepared_commit =
      index_writer.prepare_commit()?;
    prepared_commit.set_payload("TEST");
    prepared_commit.commit()?;
  }
  Ok(())
}
