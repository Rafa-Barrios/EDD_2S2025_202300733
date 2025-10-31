unit userMessageDraft;

{$MODE DELPHI}

interface

uses
  gtk2, glib2, SysUtils,
  variables, doubleLinkedList, jsonTools, interfaceTools, userDraft,
  fpjson, jsonparser;

procedure ShowUserMessageDraft(mailID: string; remitente, destinatario, asunto, cuerpo: string);

implementation

var
  msgWindow: PGtkWidget;
  btnClose, btnSend, btnDelete: PGtkWidget; // ✅ botón "Eliminar"
  txtTo, txtSubject: PGtkWidget;
  txtBody: PGtkWidget;
  buffer: PGtkTextBuffer;

  currentID: string;
  currentRemitente, currentDestinatario: string;

// ------------------------------
// Evento cerrar
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(msgWindow);
  ReloadDrafts;
end;

// ------------------------------
// Evento enviar
// ------------------------------
procedure OnSendClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  start_iter, end_iter: TGtkTextIter;
  newSubject, newBody: string;
  destinatario: string;
  jsonData: TJSONData = nil;
  jsonObject, draftObj: TJSONObject;
  draftsArray: TJSONArray;
  i: Integer;
begin
  // Obtener asunto
  newSubject := gtk_entry_get_text(GTK_ENTRY(txtSubject));

  // Obtener cuerpo
  gtk_text_buffer_get_start_iter(buffer, @start_iter);
  gtk_text_buffer_get_end_iter(buffer, @end_iter);
  newBody := gtk_text_buffer_get_text(buffer, @start_iter, @end_iter, True);

  // Leer destinatario desde drafts.json
  destinatario := currentDestinatario;
  if FileExists(json_file_drafts) then
  begin
    jsonData := GetJSON(ReadFileToString(json_file_drafts));
    jsonObject := TJSONObject(jsonData);
    draftsArray := jsonObject.Arrays['mails'];

    for i := 0 to draftsArray.Count - 1 do
    begin
      draftObj := draftsArray.Objects[i];
      if draftObj.Integers['id'] = StrToIntDef(currentID, 0) then
      begin
        destinatario := draftObj.Get('destinatario', currentDestinatario);
        Break;
      end;
    end;
  end;

  // Guardar en mails.json y eliminar de drafts.json
  if (destinatario <> '') and
     AddMailToJson(json_file_mails, current_user_email, destinatario,
                   newSubject, newBody, StrToIntDef(currentID, 0)) then
  begin
    RemoveDraftFromJson(json_file_drafts, StrToIntDef(currentID, 0));
  end;

  gtk_widget_destroy(msgWindow);
  ReloadDrafts;

  if Assigned(jsonData) then jsonData.Free;
end;

// ------------------------------
// ✅ Evento eliminar borrador (versión corregida)
// ------------------------------
procedure OnDeleteClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  success: Boolean;
  draftID: Integer;
begin
  draftID := StrToIntDef(currentID, -1);

  if draftID <> -1 then
  begin
    success := MoveMailToTrash(json_file_drafts, json_file_trash,
                               current_user_email, draftID);

    if success then
      Writeln('Borrador movido a la papelera correctamente.')
    else
      Writeln('Error al mover el borrador a la papelera.');
  end
  else
    Writeln('Error: ID de borrador inválido.');

  gtk_widget_destroy(msgWindow);
  ReloadDrafts;
end;

// ------------------------------
// Mostrar mensaje en borrador
// ------------------------------
procedure ShowUserMessageDraft(mailID: string; remitente, destinatario, asunto, cuerpo: string);
var
  vbox, hbox, hboxButtons: PGtkWidget;
  scrolled: PGtkWidget;
  lbl: PGtkWidget;
begin
  currentID := mailID;
  currentRemitente := remitente;
  currentDestinatario := destinatario;

  msgWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(msgWindow), 'Borrador');
  gtk_container_set_border_width(GTK_CONTAINER(msgWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(msgWindow), 600, 500);

  vbox := gtk_vbox_new(False, 8);
  gtk_container_add(GTK_CONTAINER(msgWindow), vbox);

  // Destinatario
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('Para:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);

  txtTo := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtTo), PChar(currentDestinatario));
  gtk_editable_set_editable(GTK_EDITABLE(txtTo), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtTo, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // Asunto
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('Asunto:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);

  txtSubject := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtSubject), PChar(asunto));
  gtk_box_pack_start(GTK_BOX(hbox), txtSubject, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // Cuerpo
  lbl := gtk_label_new('Mensaje:');
  gtk_box_pack_start(GTK_BOX(vbox), lbl, False, False, 0);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, True, True, 0);

  txtBody := gtk_text_view_new;
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(txtBody), GTK_WRAP_WORD);
  gtk_container_add(GTK_CONTAINER(scrolled), txtBody);

  buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(txtBody));
  gtk_text_buffer_set_text(buffer, PChar(cuerpo), -1);

  // Botones
  hboxButtons := gtk_hbox_new(True, 10);

  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnClose, True, True, 0);

  btnSend := gtk_button_new_with_label('Enviar');
  g_signal_connect(btnSend, 'clicked', G_CALLBACK(@OnSendClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnSend, True, True, 0);

  // ✅ Nuevo botón "Eliminar"
  btnDelete := gtk_button_new_with_label('Eliminar');
  g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@OnDeleteClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnDelete, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hboxButtons, False, False, 0);

  gtk_widget_show_all(msgWindow);
end;

end.


