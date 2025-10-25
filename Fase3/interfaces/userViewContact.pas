unit userViewContact;

{$mode objfpc}{$H+}

interface

procedure ShowUserViewContactWindow;

implementation

uses
  gtk2, glib2, SysUtils, circularLinkedList, userHome, variables;

var
  viewWindow: PGtkWidget;

  lblName, lblUsername, lblEmail, lblPhone: PGtkWidget;
  txtName, txtUsername, txtEmail, txtPhone: PGtkWidget;

  btnPrev, btnNext, btnBack: PGtkWidget;

  currentContact: PCNode;

// -------- Diálogo simple de información --------
procedure ShowInfoMessage(const title, message: string);
var
  dialog: PGtkWidget;
begin
  dialog := gtk_message_dialog_new(nil,
                                   GTK_DIALOG_MODAL,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   PChar(message));
  gtk_window_set_title(GTK_WINDOW(dialog), PChar(title));
  gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
end;
// -----------------------------------------------

// Actualizar visualización del contacto
procedure UpdateContactDisplay;
begin
  if currentContact = nil then
  begin
    gtk_entry_set_text(GTK_ENTRY(txtName), '');
    gtk_entry_set_text(GTK_ENTRY(txtUsername), '');
    gtk_entry_set_text(GTK_ENTRY(txtEmail), '');
    gtk_entry_set_text(GTK_ENTRY(txtPhone), '');
    Exit;
  end;

  gtk_entry_set_text(GTK_ENTRY(txtName), PChar(currentContact^.nombre));
  gtk_entry_set_text(GTK_ENTRY(txtUsername), PChar(currentContact^.id));
  gtk_entry_set_text(GTK_ENTRY(txtEmail), PChar(currentContact^.email));
  gtk_entry_set_text(GTK_ENTRY(txtPhone), PChar(currentContact^.telefono));
end;

// Botón Siguiente
procedure OnNextClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if currentContact <> nil then
  begin
    currentContact := currentContact^.siguiente;
    UpdateContactDisplay;
  end;
end;

// Botón Anterior
procedure OnPrevClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  temp: PCNode;
begin
  if currentContact = nil then Exit;

  temp := currentContact;
  while temp^.siguiente <> currentContact do
    temp := temp^.siguiente;

  currentContact := temp;
  UpdateContactDisplay;
end;

// Botón Atrás
procedure OnBackClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(viewWindow);
  ShowUserHomeWindow;
end;

// Mostrar ventana de contactos
procedure ShowUserViewContactWindow;
var
  vbox, hboxButtons, grid: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  // Si no hay contactos en la lista del usuario actual → aviso y no abrimos ventana
  if current_user_contacts = nil then
  begin
    ShowInfoMessage('Información', 'Este usuario no tiene contactos agregados.');
    Exit;
  end;

  currentContact := current_user_contacts;

  viewWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(viewWindow), 'Ver Contacto');
  gtk_container_set_border_width(GTK_CONTAINER(viewWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(viewWindow), 450, 250);

  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(viewWindow), vbox);

  // Grid para etiquetas y cajas de texto
  grid := gtk_table_new(4, 2, False);
  gtk_box_pack_start(GTK_BOX(vbox), grid, False, False, 5);

  // Nombre
  lblName := gtk_label_new('Nombre:');
  gtk_misc_set_alignment(GTK_MISC(lblName), 0, 0.5);
  txtName := gtk_entry_new;
  gtk_editable_set_editable(GTK_EDITABLE(txtName), False);
  gtk_table_attach(GTK_TABLE(grid), lblName, 0, 1, 0, 1, GTK_FILL, GTK_FILL, 5, 5);
  gtk_table_attach(GTK_TABLE(grid), txtName, 1, 2, 0, 1, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  // Usuario
  lblUsername := gtk_label_new('Usuario:');
  gtk_misc_set_alignment(GTK_MISC(lblUsername), 0, 0.5);
  txtUsername := gtk_entry_new;
  gtk_editable_set_editable(GTK_EDITABLE(txtUsername), False);
  gtk_table_attach(GTK_TABLE(grid), lblUsername, 0, 1, 1, 2, GTK_FILL, GTK_FILL, 5, 5);
  gtk_table_attach(GTK_TABLE(grid), txtUsername, 1, 2, 1, 2, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  // Correo
  lblEmail := gtk_label_new('Correo:');
  gtk_misc_set_alignment(GTK_MISC(lblEmail), 0, 0.5);
  txtEmail := gtk_entry_new;
  gtk_editable_set_editable(GTK_EDITABLE(txtEmail), False);
  gtk_table_attach(GTK_TABLE(grid), lblEmail, 0, 1, 2, 3, GTK_FILL, GTK_FILL, 5, 5);
  gtk_table_attach(GTK_TABLE(grid), txtEmail, 1, 2, 2, 3, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  // Teléfono
  lblPhone := gtk_label_new('Teléfono:');
  gtk_misc_set_alignment(GTK_MISC(lblPhone), 0, 0.5);
  txtPhone := gtk_entry_new;
  gtk_editable_set_editable(GTK_EDITABLE(txtPhone), False);
  gtk_table_attach(GTK_TABLE(grid), lblPhone, 0, 1, 3, 4, GTK_FILL, GTK_FILL, 5, 5);
  gtk_table_attach(GTK_TABLE(grid), txtPhone, 1, 2, 3, 4, GTK_EXPAND or GTK_FILL, GTK_FILL, 5, 5);

  UpdateContactDisplay;

  // Botones
  hboxButtons := gtk_hbox_new(True, 10);
  gtk_box_pack_start(GTK_BOX(vbox), hboxButtons, False, False, 5);

  btnPrev := gtk_button_new_with_label('Anterior');
  btnBack := gtk_button_new_with_label('Atrás');
  btnNext := gtk_button_new_with_label('Siguiente');

  gtk_box_pack_start(GTK_BOX(hboxButtons), btnPrev, True, True, 2);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnBack, True, True, 2);
  gtk_box_pack_start(GTK_BOX(hboxButtons), btnNext, True, True, 2);

  g_signal_connect(btnPrev, 'clicked', G_CALLBACK(@OnPrevClick), nil);
  g_signal_connect(btnNext, 'clicked', G_CALLBACK(@OnNextClick), nil);
  g_signal_connect(btnBack, 'clicked', G_CALLBACK(@OnBackClick), nil);
  g_signal_connect(viewWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_widget_show_all(viewWindow);
  gtk_main;
end;

end.