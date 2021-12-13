use crate::api;
use crate::core::document::Language;
use crate::core::hex::ToHex as _;
use crate::core::result::DynResult;
use crate::search;
use tokio::task::spawn_blocking;

#[derive(Debug, serde::Deserialize)]
struct Params {
  language: Language,
  query: String,
}

pub async fn handle(
  api_context: api::Context,
  search_context_map: search::ContextMap,
  req: hyper::Request<hyper::Body>,
) -> DynResult<hyper::Response<hyper::Body>> {
  let permit = api_context.cpu_available.acquire().await?;
  let documents = spawn_blocking(move || {
    let params: Params = serde_urlencoded::from_str(
      req.uri().query().unwrap_or(""),
    )?;
    let search_context_map = search_context_map.as_ref();
    let search_context = search_context_map
      .get(&params.language)
      .ok_or("invalid language")?;
    search::execute_query(search_context, &params.query)
  })
  .await??;
  drop(permit);
  Ok(hyper::Response::new(
    serde_json::to_string(
      &documents.into_iter().map(|document| serde_json::json!({
        "type": document.type_,
        "id": document.id.to_hex(),
        "created_at": document.created_at.unix_timestamp(),
        "created_by": document.created_by.to_hex(),
        "updated_at": document.updated_at.unix_timestamp(),
        "subforum_id": document.subforum_id.map(|value| value.to_hex()),
        "topic_id": document.topic_id.map(|value| value.to_hex()),
        "taxon_rank": document.taxon_rank,
        "parents": document.parents.map(|values| values.into_iter().map(|value| value.to_hex()).collect::<Vec<String>>()),
        "title": document.title,
        "names": document.names,
        "tags": document.tags.into_iter().map(|value| value.to_hex()).collect::<Vec<String>>(),
        "content": document.content
      })).collect::<Vec<serde_json::Value>>()
    )?.into(),
  ))
}
