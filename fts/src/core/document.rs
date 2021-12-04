use time::OffsetDateTime;

#[derive(
  Debug,
  Clone,
  Copy,
  PartialEq,
  Eq,
  Hash,
  strum_macros::EnumIter,
  strum_macros::AsRefStr,
  strum_macros::EnumString,
  postgres_types::ToSql,
  postgres_types::FromSql,
)]
#[postgres(name = "language")]
pub enum Language {
  #[postgres(name = "en")]
  #[strum(serialize = "en")]
  English,
  #[postgres(name = "fr")]
  #[strum(serialize = "fr")]
  French,
  #[postgres(name = "ja")]
  #[strum(serialize = "ja")]
  Japanese,
}

#[derive(
  Debug, postgres_types::ToSql, postgres_types::FromSql,
)]
#[postgres(name = "taxon_rank")]
pub enum TaxonRank {
  #[postgres(name = "kingdom")]
  Kingdom,
  #[postgres(name = "family")]
  Family,
  #[postgres(name = "genus")]
  Genus,
  #[postgres(name = "species")]
  Species,
  #[postgres(name = "variant")]
  Variant,
}

#[derive(Debug)]
pub struct Document {
  pub type_: String,
  pub id: Vec<u8>,
  pub created_at: OffsetDateTime,
  pub created_by: Vec<u8>,
  pub updated_at: OffsetDateTime,
  pub deleted: bool,
  pub language: Language,
  pub subforum_id: Option<Vec<u8>>,
  pub topic_id: Option<Vec<u8>>,
  pub taxon_rank: Option<TaxonRank>,
  pub title: Option<String>,
  pub names: Option<Vec<String>>,
  pub tags: Option<Vec<Vec<u8>>>,
  pub content: String,
}
