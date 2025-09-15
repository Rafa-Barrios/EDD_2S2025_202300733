{$MODE DELPHI}

unit rootComunidad;

interface
procedure ShowRootComunidadWindow;

implementation

uses
  gtk2, glib2, gdk2, SysUtils, linkedListOfLists, interfaceTools, jsonTools,
  rootHome;  // 🔹 Importamos rootHome para poder volver

var
  comunidadWindow: PGtkWidget;
  entryNuevaComunidad, entryComunidad, entryCorreo: PGtkWidget;
  btnCrear, btnAgregar, btnCancelar: PGtkWidget;

// ------------------------------
// Crear nueva comunidad
// ------------------------------
procedure OnCrearClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  nombre: string;
begin
  nombre := Trim(gtk_entry_get_text(GTK_ENTRY(entryNuevaComunidad)));

  if nombre = '' then
  begin
    ShowErrorMessage(comunidadWindow, 'Crear Comunidad', 'Ingrese un nombre de comunidad válido.');
    Exit;
  end;

  // Insertar en la lista de listas
  LL_InsertHeader(nombre);

  ShowSuccessMessage(comunidadWindow, 'Crear Comunidad', 
    'Comunidad "' + nombre + '" creada correctamente.');

  // Limpiar entrada
  gtk_entry_set_text(GTK_ENTRY(entryNuevaComunidad), '');
end;

// ------------------------------
// Agregar correo a comunidad
// ------------------------------
procedure OnAgregarClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  comunidad, correo: string;
begin
  comunidad := Trim(gtk_entry_get_text(GTK_ENTRY(entryComunidad)));
  correo := Trim(gtk_entry_get_text(GTK_ENTRY(entryCorreo)));

  if (comunidad = '') or (correo = '') then
  begin
    ShowErrorMessage(comunidadWindow, 'Agregar Correo', 
      'Complete ambos campos: Comunidad y Correo.');
    Exit;
  end;

  // Insertar correo directamente en la comunidad
  LL_InsertElement(comunidad, correo);

  ShowSuccessMessage(comunidadWindow, 'Agregar Correo', 
    'Correo "' + correo + '" agregado a la comunidad "' + comunidad + '".');

  // Limpiar entradas
  gtk_entry_set_text(GTK_ENTRY(entryComunidad), '');
  gtk_entry_set_text(GTK_ENTRY(entryCorreo), '');
end;

// ------------------------------
// Cancelar y volver a RootHome
// ------------------------------
procedure OnCancelarClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(comunidadWindow);  // 🔹 Cierra ventana de comunidades
  ShowRootHomeWindow;                   // 🔹 Regresa a rootHome
end;

// ------------------------------
// Mostrar ventana Root Comunidad
// ------------------------------
procedure ShowRootComunidadWindow;
var
  vbox, hboxNueva, hboxComunidad, hboxCorreo, hboxBotones: PGtkWidget;
  lblNueva, lblComunidad, lblCorreo: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  // Ventana principal
  comunidadWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(comunidadWindow), 'Comunidades');
  gtk_container_set_border_width(GTK_CONTAINER(comunidadWindow), 20);
  gtk_window_set_default_size(GTK_WINDOW(comunidadWindow), 400, 260);

  // Contenedor vertical principal
  vbox := gtk_vbox_new(False, 10);
  gtk_container_add(GTK_CONTAINER(comunidadWindow), vbox);

  // Sección Nueva Comunidad
  hboxNueva := gtk_hbox_new(False, 5);
  lblNueva := gtk_label_new('Nueva Comunidad:');
  entryNuevaComunidad := gtk_entry_new;
  gtk_box_pack_start(GTK_BOX(hboxNueva), lblNueva, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hboxNueva), entryNuevaComunidad, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxNueva, False, False, 0);

  btnCrear := gtk_button_new_with_label('Crear');
  g_signal_connect(btnCrear, 'clicked', G_CALLBACK(@OnCrearClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnCrear, False, False, 0);

  // Sección Comunidad + Correo
  hboxComunidad := gtk_hbox_new(False, 5);
  lblComunidad := gtk_label_new('Comunidad:');
  entryComunidad := gtk_entry_new;
  gtk_box_pack_start(GTK_BOX(hboxComunidad), lblComunidad, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hboxComunidad), entryComunidad, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxComunidad, False, False, 0);

  hboxCorreo := gtk_hbox_new(False, 5);
  lblCorreo := gtk_label_new('Correo:');
  entryCorreo := gtk_entry_new;
  gtk_box_pack_start(GTK_BOX(hboxCorreo), lblCorreo, False, False, 0);
  gtk_box_pack_start(GTK_BOX(hboxCorreo), entryCorreo, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxCorreo, False, False, 0);

  btnAgregar := gtk_button_new_with_label('Agregar');
  g_signal_connect(btnAgregar, 'clicked', G_CALLBACK(@OnAgregarClick), nil);
  gtk_box_pack_start(GTK_BOX(vbox), btnAgregar, False, False, 0);

  // 🔹 Nueva sección de botones de control (Cancelar)
  hboxBotones := gtk_hbox_new(True, 5);
  btnCancelar := gtk_button_new_with_label('Cancelar');
  g_signal_connect(btnCancelar, 'clicked', G_CALLBACK(@OnCancelarClick), nil);
  gtk_box_pack_start(GTK_BOX(hboxBotones), btnCancelar, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), hboxBotones, False, False, 0);

  // Mostrar todo
  gtk_widget_show_all(comunidadWindow);

  g_signal_connect(comunidadWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  gtk_main;
end;

end.
