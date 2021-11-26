use std::error::Error as StdError;
use std::fs;
use std::path::Path;

pub struct Context {
  pub schema: tantivy::schema::Schema,
  pub index: tantivy::Index,
  pub index_reader: tantivy::IndexReader,
  pub fields: FieldSet,
}

pub struct FieldSet {
  pub title: tantivy::schema::Field,
  pub body: tantivy::schema::Field,
}

pub fn load() -> Result<Context, Box<dyn StdError>> {
  let index_path = Path::new("./.tantivy/topic");
  fs::create_dir_all(index_path)?;
  let index_dir =
    tantivy::directory::MmapDirectory::open(index_path)?;
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
  let index = tantivy::Index::open_or_create(
    index_dir,
    schema.clone(),
  )?;
  let tokenizer = tantivy::tokenizer::TextAnalyzer::from(
    tantivy::tokenizer::SimpleTokenizer,
  )
  .filter(tantivy::tokenizer::LowerCaser)
  .filter(tantivy::tokenizer::Stemmer::new(
    tantivy::tokenizer::Language::English,
  ));
  index.tokenizers().register("basic", tokenizer);
  let index_meta = index.load_metas()?;
  if index_meta.payload != Some("TEST".to_string()) {
    let mut index_writer = index.writer(50_000_000)?;
    let mut old_man_doc =
      tantivy::schema::Document::default();
    old_man_doc
      .add_text(fields.title, "The Old Man and the Sea");
    old_man_doc.add_text(
        fields.body,
        "He was an old man who fished alone in a skiff in the Gulf Stream and \
         he had gone eighty-four days now without taking a fish.",
    );

    // ... and add it to the `IndexWriter`.
    index_writer.add_document(old_man_doc);
    index_writer.add_document(tantivy::doc!(
    fields.title => "Of Mice and Men",
    fields.body => "A few miles south of Soledad, the Salinas River drops in close to the hillside \
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
      fields.body => "You will rejoice to hear that no disaster has accompanied the commencement of an \
               enterprise which you have regarded with such evil forebodings.  I arrived here \
               yesterday, and my first task is to assure my dear sister of my welfare and \
               increasing confidence in the success of my undertaking."
      ));
    let mut prepared_commit =
      index_writer.prepare_commit()?;
    prepared_commit.set_payload("TEST");
    prepared_commit.commit()?;
  }

  let index_reader = index
    .reader_builder()
    .reload_policy(tantivy::ReloadPolicy::OnCommit)
    .try_into()?;

  Ok(Context {
    schema,
    index,
    index_reader,
    fields,
  })
}
