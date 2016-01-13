%%%-------------------------------------------------------------------
%%% @author sanr
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. апр 2014 11:50
%%%-------------------------------------------------------------------
-author("sanr").
-record(crm_users, {
  id,
  asterisk_ext_c,
  asterisk_missed_calls_c,
  asterisk_call_group_c
}).

-record(contact, {
  id = null,
  name = "Не указано",
  type,
  def_assigned = false,
  assigned_user_id
}).

-record(internal_call, {
  call_id,
  number,
  client_name,
  crm_client_id,
  crm_call_id,
  manager_num,
  title,
  description,
  type,
  tag,
  crm_client_type
}).