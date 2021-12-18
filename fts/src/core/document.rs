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
  postgres_types::ToSql,
  postgres_types::FromSql,
  serde::Deserialize,
)]
#[postgres(name = "language")]
pub enum Language {
  #[postgres(name = "en")]
  #[strum(serialize = "en")]
  #[serde(rename = "en")]
  English,
  #[postgres(name = "fr")]
  #[strum(serialize = "fr")]
  #[serde(rename = "fr")]
  French,
  #[postgres(name = "ja")]
  #[strum(serialize = "ja")]
  #[serde(rename = "ja")]
  Japanese,
}

#[derive(
  Debug,
  postgres_types::ToSql,
  postgres_types::FromSql,
  strum_macros::AsRefStr,
  strum_macros::EnumString,
  serde::Serialize,
  serde::Deserialize,
)]
#[postgres(name = "taxon_rank")]
pub enum TaxonRank {
  #[postgres(name = "kingdom")]
  #[strum(serialize = "kingdom")]
  #[serde(rename = "kingdom")]
  Kingdom,
  #[postgres(name = "family")]
  #[strum(serialize = "family")]
  #[serde(rename = "family")]
  Family,
  #[postgres(name = "genus")]
  #[strum(serialize = "genus")]
  #[serde(rename = "genus")]
  Genus,
  #[postgres(name = "species")]
  #[strum(serialize = "species")]
  #[serde(rename = "species")]
  Species,
  #[postgres(name = "variant")]
  #[strum(serialize = "variant")]
  #[serde(rename = "variant")]
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
  pub subforum_id: Option<Vec<u8>>,
  pub topic_id: Option<Vec<u8>>,
  pub taxon_rank: Option<TaxonRank>,
  pub parents: Vec<Vec<u8>>,
  pub title: Option<String>,
  pub names: Vec<String>,
  pub tags: Vec<Vec<u8>>,
  pub content: String,
}
