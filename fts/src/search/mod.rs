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
  pub title: tantivy::schema::Field,
  pub body: tantivy::schema::Field,
}

pub fn load() -> Result<Context, Box<dyn StdError>> {
  let (schema, fields) = build_schema();
  let instances = Language::iter()
    .map(|language| {
      load_index(&schema, language)
        .map(|instance| (language, instance))
    })
    .collect::<Result<
      HashMap<Language, Instance>,
      Box<dyn StdError>,
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
  let text_indexing_options = tantivy::schema::TextFieldIndexing::default()
    .set_tokenizer("basic")
    .set_index_option(
      tantivy::schema::IndexRecordOption::WithFreqsAndPositions,
    );
  let text_options =
    tantivy::schema::TextOptions::default()
      .set_indexing_options(text_indexing_options)
      .set_stored();
  let fields = FieldSet {
    title: schema_builder
      .add_text_field("title", text_options.clone()),
    body: schema_builder
      .add_text_field("body", text_options),
  };
  let schema = schema_builder.build();
  (schema, fields)
}

pub fn load_index(
  schema: &tantivy::schema::Schema,
  language: Language,
) -> Result<Instance, Box<dyn StdError>> {
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

pub fn update(
  search_context: &Context,
) -> Result<(), Box<dyn StdError>> {
  let instance = search_context
    .instances
    .get(&Language::Japanese)
    .ok_or("Invalid language")?;
  let index_meta = instance.index.load_metas()?;
  if index_meta.payload != Some("TEST".to_string()) {
    let mut index_writer =
      instance.index.writer(50_000_000)?;
    let mut old_man_doc =
      tantivy::schema::Document::default();
    old_man_doc.add_text(
      search_context.fields.title,
      "The Old Man and the Sea",
    );
    old_man_doc.add_text(
        search_context.fields.body,
        "He was an old man who fished alone in a skiff in the Gulf Stream and \
         he had gone eighty-four days now without taking a fish.",
    );

    // ... and add it to the `IndexWriter`.
    index_writer.add_document(old_man_doc);
    index_writer.add_document(tantivy::doc!(
    search_context.fields.title => "Of Mice and Men",
    search_context.fields.body => "A few miles south of Soledad, the Salinas River drops in close to the hillside \
            bank and runs deep and green. The water is warm too, for it has slipped twinkling \
            over the yellow sands in the sunlight before reaching the narrow pool. On one \
            side of the river the golden foothill slopes curve up to the strong and rocky \
            Gabilan Mountains, but on the valley side the water is lined with trees—willows \
            fresh and green with every spring, carrying in their lower leaf junctures the \
            debris of the winter’s flooding; and sycamores with mottled, white, recumbent \
            limbs and branches that arch over the pool"
    ));
    index_writer.add_document(tantivy::doc!(
      search_context.fields.title => "Frankenstein",
      search_context.fields.title => "The Modern Prometheus",
      search_context.fields.body => "You will rejoice to hear that no disaster has accompanied the commencement of an \
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
