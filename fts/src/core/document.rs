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
