unit userViewContact;

{$mode objfpc}{$H+}

interface

procedure ShowUserViewContactWindow;

implementation

uses
  gtk2, glib2, SysUtils, circularLinkedList, userHome;

var
  viewWindow: PGtkWidget;

  lblName, lblUsername, lblEmail, lblPhone: PGtkWidget;
  btnPrev, btnNext, btnBack: PGtkWidget;

  currentContact: PCNode;

procedure UpdateContactDisplay;
begin
  if currentContact = nil then Exit;

  gtk_label_set_text(GTK_LABEL(lblName), PChar('Nombre: ' + currentContact^.nombre));
  gtk_label_set_text(GTK_LABEL(lblUsername), PChar('Usuario: ' + currentContact^.id));
  gtk_label_set_text(GTK_LABEL(lblEmail), PChar('Correo: ' + currentContact^.email));
  gtk_label_set_text(GTK_LABEL(lblPhone), PChar('Teléfono: ' + currentContact^.id));
end;

// Botón Siguiente
procedure OnNextClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  if currentContact <> nil then
    currentContact := currentContact^.siguiente;
  UpdateContactDisplay;
end;

// Botón Anterior
procedure OnPrevClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  temp: PCNode;
begin
  if currentContact = nil then Exit;
  temp := currentContact;

  // recorrer toda la lista hasta encontrar el nodo anterior
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

// Mostrar ventana
procedure ShowUserViewContactWindow;
var
  vbox, hboxButtons: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  if CL_Head = nil then Exit; // no hay contactos

  currentContact := CL_Head;

  viewWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(viewWindow), 'Ver Contacto');
  gtk_container_set_border_width(GTK_CONTAINER(viewWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(viewWindow), 400, 200);

  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(viewWindow), vbox);

  // Labels de contacto
  lblName := gtk_label_new('');
  lblUsername := gtk_label_new('');
  lblEmail := gtk_label_new('');
  lblPhone := gtk_label_new('');

  gtk_box_pack_start(GTK_BOX(vbox), lblName, False, False, 2);
  gtk_box_pack_start(GTK_BOX(vbox), lblUsername, False, False, 2);
  gtk_box_pack_start(GTK_BOX(vbox), lblEmail, False, False, 2);
  gtk_box_pack_start(GTK_BOX(vbox), lblPhone, False, False, 2);

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
