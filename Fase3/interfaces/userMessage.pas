unit userMessage;

{$MODE DELPHI}

interface

uses
  gtk2, glib2, SysUtils,
  doubleLinkedList, userInbox, jsonTools, variables, lzwTools;

procedure ShowUserMessageWindow(mail: PMailNode);

implementation

var
  msgWindow: PGtkWidget;
  btnClose, btnDelete, btnFavorite, btnDownload: PGtkWidget;
  txtFrom, txtSubject, txtDate: PGtkWidget;
  txtBody: PGtkWidget;
  currentMail: PMailNode;

// ------------------------------
// Evento cerrar mensaje
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(msgWindow);
  ReloadInbox;
end;

// ------------------------------
// Evento eliminar mensaje
// ------------------------------
procedure OnDeleteClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  success: Boolean;
  mailID: Integer;
begin
  if currentMail <> nil then
  begin
    // ✅ Convertir el ID de correo a entero por seguridad
    mailID := StrToIntDef(currentMail^.id, 0);

    success := MoveMailToTrash(json_file_mails, json_file_trash,
                               current_user_email, mailID);

    if success then
      Writeln('Correo movido a la papelera correctamente.')
    else
      Writeln('Error al mover el correo a la papelera.');

    gtk_widget_destroy(msgWindow);
    ReloadInbox;
  end;
end;

// ------------------------------
// Evento mover a favoritos
// ------------------------------
procedure OnFavoriteClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  success: Boolean;
  mailID: Integer;
begin
  if currentMail <> nil then
  begin
    // ✅ Igual aquí, aseguramos conversión segura
    mailID := StrToIntDef(currentMail^.id, 0);

    success := MoveMailToTrash(json_file_mails, json_file_favorites,
                               current_user_email, mailID);

    if success then
      Writeln('Correo movido a favoritos correctamente.')
    else
      Writeln('Error al mover el correo a favoritos.');

    gtk_widget_destroy(msgWindow);
    ReloadInbox;
  end;
end;

// ------------------------------
// Evento descargar mensaje (solo cuerpo)
// ------------------------------
procedure OnDownloadClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  safeSubject, filePath: string;
begin
  if currentMail = nil then Exit;

  // Sanitizar nombre
  safeSubject := StringReplace(currentMail^.subject, ' ', '_', [rfReplaceAll]);
  safeSubject := StringReplace(safeSubject, '/', '-', [rfReplaceAll]);
  safeSubject := StringReplace(safeSubject, '\', '-', [rfReplaceAll]);

  // Guardar mensaje comprimido en carpeta data/
  filePath := variables.test_folder + safeSubject + '_mensaje.txt';
  CompressMessageToFile(currentMail^.body, filePath);

  Writeln('Mensaje comprimido y guardado en: ', filePath);
end;

// ------------------------------
// Mostrar ventana de mensaje
// ------------------------------
procedure ShowUserMessageWindow(mail: PMailNode);
var
  vbox, hbox, hboxButtons: PGtkWidget;
  scrolled: PGtkWidget;
  buffer: PGtkTextBuffer;
  lbl: PGtkWidget;
begin
  if mail = nil then Exit;

  currentMail := mail;

  // Marcar como leído
  if mail^.estado = 'NL' then
    mail^.estado := 'L';

  // Crear ventana
  msgWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(msgWindow), 'Mensaje');
  gtk_container_set_border_width(GTK_CONTAINER(msgWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(msgWindow), 600, 500);

  vbox := gtk_vbox_new(False, 8);
  gtk_container_add(GTK_CONTAINER(msgWindow), vbox);

  // Remitente
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('De:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);
  txtFrom := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtFrom), PChar(mail^.sender));
  gtk_editable_set_editable(GTK_EDITABLE(txtFrom), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtFrom, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // Asunto
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('Asunto:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);
  txtSubject := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtSubject), PChar(mail^.subject));
  gtk_editable_set_editable(GTK_EDITABLE(txtSubject), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtSubject, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // Fecha
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('Fecha:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);
  txtDate := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtDate), PChar(mail^.timestamp));
  gtk_editable_set_editable(GTK_EDITABLE(txtDate), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtDate, True, True, 0);
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
  gtk_text_view_set_editable(GTK_TEXT_VIEW(txtBody), False);
  gtk_container_add(GTK_CONTAINER(scrolled), txtBody);
  buffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(txtBody));
  gtk_text_buffer_set_text(buffer, PChar(mail^.body), -1);

  // Botones
  hboxButtons := gtk_hbox_new(True, 10);

  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnClose, True, True, 0);

  btnDelete := gtk_button_new_with_label('Eliminar');
  g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@OnDeleteClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnDelete, True, True, 0);

  btnFavorite := gtk_button_new_with_label('Favoritos');
  g_signal_connect(btnFavorite, 'clicked', G_CALLBACK(@OnFavoriteClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnFavorite, True, True, 0);

  btnDownload := gtk_button_new_with_label('Descargar');
  g_signal_connect(btnDownload, 'clicked', G_CALLBACK(@OnDownloadClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnDownload, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hboxButtons, False, False, 0);
  gtk_widget_show_all(msgWindow);
end;

end.


