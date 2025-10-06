unit userCommunitySend;

{$MODE DELPHI}

interface

uses
  gtk2, glib2, SysUtils,
  variables, userHome, jsonTools, interfaceTools; // <-- añadimos jsonTools

procedure ShowUserCommunitySendWindow;

implementation

var
  communityWindow: PGtkWidget;
  entryCommunityName: PGtkWidget;
  textViewMessage: PGtkWidget;
  btnSend, btnCancel: PGtkWidget;
  txtBuffer: PGtkTextBuffer;

// ------------------------------
// Cancelar -> cierra y vuelve a Home
// ------------------------------
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(communityWindow);
  ShowUserHomeWindow;
end;

// ------------------------------
// Enviar -> crea o actualiza com.json
// ------------------------------
procedure OnSendClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  communityName, messageText: string;
  startIter, endIter: TGtkTextIter;
begin
  // Leer nombre de comunidad
  communityName := gtk_entry_get_text(GTK_ENTRY(entryCommunityName));

  // Leer mensaje del textview
  gtk_text_buffer_get_start_iter(txtBuffer, @startIter);
  gtk_text_buffer_get_end_iter(txtBuffer, @endIter);
  messageText := gtk_text_buffer_get_text(txtBuffer, @startIter, @endIter, True);

  // Validaciones básicas
  if (Trim(communityName) = '') or (Trim(messageText) = '') then
  begin
    //ShowMessage('Debe ingresar el nombre de la comunidad y un mensaje.');
    Exit;
  end;

  // Llamar a jsonTools para guardar el mensaje
  try
    SaveCommunityMessage(communityName, messageText, current_user_email);

    ShowSuccessMessage(communityWindow, 'Éxito', 'Mensaje enviado correctamente a la comunidad "' + communityName + '".');
    gtk_widget_destroy(communityWindow);
    ShowUserHomeWindow;
  except
    on E: Exception do
      ShowErrorMessage(communityWindow, 'Error', 'Debe ingresar el nombre de la comunidad y un mensaje.');
  end;
end;

// ------------------------------
// Mostrar ventana de envío a comunidad
// ------------------------------
procedure ShowUserCommunitySendWindow;
var
  vbox, hboxLabel, hboxButtons: PGtkWidget;
  lblName, lblMessage: PGtkWidget;
  scrolled: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  // Ventana principal
  communityWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(communityWindow), 'Enviar mensaje a Comunidad');
  gtk_container_set_border_width(GTK_CONTAINER(communityWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(communityWindow), 600, 400);

  // Contenedor vertical principal
  vbox := gtk_vbox_new(False, 8);
  gtk_container_add(GTK_CONTAINER(communityWindow), vbox);

  // -------------------------
  // Nombre de la comunidad
  // -------------------------
  hboxLabel := gtk_hbox_new(False, 5);
  lblName := gtk_label_new('Comunidad:');
  gtk_box_pack_start(GTK_BOX(hboxLabel), lblName, False, False, 0);

  entryCommunityName := gtk_entry_new;
  gtk_entry_set_max_length(GTK_ENTRY(entryCommunityName), 200);
  gtk_box_pack_start(GTK_BOX(hboxLabel), entryCommunityName, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hboxLabel, False, False, 0);

  // -------------------------
  // Área de mensaje
  // -------------------------
  lblMessage := gtk_label_new('Mensaje:');
  gtk_box_pack_start(GTK_BOX(vbox), lblMessage, False, False, 0);

  scrolled := gtk_scrolled_window_new(nil, nil);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
                                 GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_set_size_request(scrolled, 580, 250);
  gtk_box_pack_start(GTK_BOX(vbox), scrolled, True, True, 0);

  textViewMessage := gtk_text_view_new;
  gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(textViewMessage), GTK_WRAP_WORD);
  gtk_container_add(GTK_CONTAINER(scrolled), textViewMessage);

  txtBuffer := gtk_text_view_get_buffer(GTK_TEXT_VIEW(textViewMessage));

  // -------------------------
  // Botones
  // -------------------------
  hboxButtons := gtk_hbox_new(True, 10);

  btnCancel := gtk_button_new_with_label('Cancelar');
  gtk_widget_set_size_request(btnCancel, 120, 40);
  g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnCancel, True, True, 0);

  btnSend := gtk_button_new_with_label('Enviar');
  gtk_widget_set_size_request(btnSend, 120, 40);
  g_signal_connect(btnSend, 'clicked', G_CALLBACK(@OnSendClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnSend, True, True, 0);

  gtk_box_pack_start(GTK_BOX(vbox), hboxButtons, False, False, 0);

  // Mostrar todo
  gtk_widget_show_all(communityWindow);
  g_signal_connect(communityWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.

