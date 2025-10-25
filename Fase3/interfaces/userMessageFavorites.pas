unit userMessageFavorites;

{$MODE DELPHI}

interface

uses
  gtk2, glib2, SysUtils,
  doubleLinkedList, userFavorites, jsonTools, variables;

procedure ShowUserMessageFavoritesWindow(mail: PMailNode);

implementation

var
  msgWindow: PGtkWidget;
  btnClose, btnDelete: PGtkWidget;
  txtFrom, txtSubject, txtDate: PGtkWidget;
  txtBody: PGtkWidget;
  currentMail: PMailNode; // correo mostrado

// ------------------------------
// Evento cerrar mensaje
// ------------------------------
procedure OnCloseClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(msgWindow);
  ReloadFavorites;
end;

// ------------------------------
// Evento eliminar mensaje (favoritos → papelera)
// ------------------------------
procedure OnDeleteClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if currentMail <> nil then
  begin
    // ✅ Movemos el correo desde favorites.json hacia trash.json
    if MoveMailToTrash(variables.json_file_favorites, variables.json_file_trash,
                        current_user_email, currentMail^.subject) then
    begin
      Writeln('Correo movido de favoritos a la papelera correctamente.');
    end
    else
    begin
      Writeln('Error al mover el correo de favoritos a la papelera.');
    end;

    // cerramos ventana y recargamos favoritos
    gtk_widget_destroy(msgWindow);
    ReloadFavorites;
  end;
end;

// ------------------------------
// Mostrar ventana de mensaje (favoritos)
// ------------------------------
procedure ShowUserMessageFavoritesWindow(mail: PMailNode);
var
  vbox, hbox, hboxButtons: PGtkWidget;
  scrolled: PGtkWidget;
  buffer: PGtkTextBuffer;
  lbl: PGtkWidget;
begin
  if mail = nil then Exit;

  currentMail := mail; // referencia para usar en eliminar

  // Crear ventana
  msgWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(msgWindow), 'Mensaje (Favoritos)');
  gtk_container_set_border_width(GTK_CONTAINER(msgWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(msgWindow), 600, 500);

  vbox := gtk_vbox_new(False, 8);
  gtk_container_add(GTK_CONTAINER(msgWindow), vbox);

  // =============================
  // Remitente
  // =============================
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('De:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);

  txtFrom := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtFrom), PChar(mail^.sender));
  gtk_editable_set_editable(GTK_EDITABLE(txtFrom), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtFrom, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // =============================
  // Asunto
  // =============================
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('Asunto:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);

  txtSubject := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtSubject), PChar(mail^.subject));
  gtk_editable_set_editable(GTK_EDITABLE(txtSubject), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtSubject, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // =============================
  // Fecha
  // =============================
  hbox := gtk_hbox_new(False, 5);
  lbl := gtk_label_new('Fecha:');
  gtk_box_pack_start(GTK_BOX(hbox), lbl, False, False, 0);

  txtDate := gtk_entry_new;
  gtk_entry_set_text(GTK_ENTRY(txtDate), PChar(mail^.timestamp));
  gtk_editable_set_editable(GTK_EDITABLE(txtDate), False);
  gtk_box_pack_start(GTK_BOX(hbox), txtDate, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hbox, False, False, 0);

  // =============================
  // Mensaje (cuerpo)
  // =============================
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

  // =============================
  // Botones (Cerrar, Eliminar) -> solo 2 botones
  // =============================
  hboxButtons := gtk_hbox_new(True, 10);

  btnClose := gtk_button_new_with_label('Cerrar');
  g_signal_connect(btnClose, 'clicked', G_CALLBACK(@OnCloseClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnClose, True, True, 0);

  btnDelete := gtk_button_new_with_label('Eliminar');
  g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@OnDeleteClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnDelete, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hboxButtons, False, False, 0);

  // =============================
  // Mostrar todo
  // =============================
  gtk_widget_show_all(msgWindow);
end;

end.
