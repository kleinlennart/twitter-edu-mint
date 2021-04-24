view_replies <- function(dat) {
  dat %>% select(
    status_id,
    user_id,
    user_name,
    text,
    created_at,
    ref_replied_to_status_id,
    is_head,
    is_reply_to_head,
    reply_count
  )
}
