{$MODE DELPHI}

unit rootHome;

interface
  procedure ShowRootHomeWindow;

implementation

uses
  gtk2, glib2, gdk2,
  login, rootComunidad,
  interfaceTools, jsonTools, variables, filesTools,
  simpleLinkedList, linkedListOfLists;

var
  rootWindow: PGtkWidget;
  btnCrearComunidad, btnMassiveLoad, btnUserReport, btnRelationReport, btnLogout: PGtkWidget;

// ------------------------------
// Redireccion a rootComunidad
// ------------------------------
procedure OnCrearComunidadClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(rootWindow);
  ShowRootComunidadWindow;
end;

// ------------------------------
// Carga masiva de usuarios desde JSON
// ------------------------------
procedure OnMassiveLoadClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  status: Boolean;
begin
  status := jsonTools.UploadUsersFromJson(json_file_path); 
  if status then
    ShowSuccessMessage(rootWindow, 'Carga de Archivo JSON', 'Los usuarios se han cargado correctamente.')
  else
    ShowErrorMessage(rootWindow, 'Carga de Archivo JSON', 'Error al cargar usuarios desde JSON.');
end;

// ------------------------------
// Genera los reportes de usuarios y comunidades
// ------------------------------
procedure OnUserReportClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  dotComunidades: string;
begin
  // 1. Usuarios
  filesTools.GenerateReports('users','Root-Reports', LSL_U_GenerateDot());

  // 2. Comunidades
  dotComunidades := linkedListOfLists.LL_GenerateDot();
  filesTools.GenerateReports('comunidades','Root-Reports', dotComunidades);

  ShowSuccessMessage(rootWindow, 'Reportes Generados', 'Se generaron los reportes de usuarios y comunidades correctamente.');
end;

// ------------------------------
// Genera reportes de relaciones
// ------------------------------
procedure OnRelationReportClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  LSL_U_PrintToConsole();
end;

// ------------------------------
// Redirecciona al login
// ------------------------------
procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(rootWindow);
  ShowLoginWindow;
end;

// ------------------------------
// Mostrar la ventana principal rootHome 
// ------------------------------
procedure ShowRootHomeWindow;
var
  vbox: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  // Crear ventana principal
  rootWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(rootWindow), 'Root Home');
  gtk_container_set_border_width(GTK_CONTAINER(rootWindow), 20);
  gtk_window_set_default_size(GTK_WINDOW(rootWindow), 400, 350);

  // VBox principal para organizar botones verticalmente
  vbox := gtk_vbox_new(False, 15);  // espacio de 15px entre botones
  gtk_container_add(GTK_CONTAINER(rootWindow), vbox);

  // Crear botones
  btnCrearComunidad := gtk_button_new_with_label('Crear Comunidad');
  btnMassiveLoad := gtk_button_new_with_label('Carga Masiva de Usuarios');
  btnUserReport := gtk_button_new_with_label('Reporte de Usuarios y Comunidades');
  btnRelationReport := gtk_button_new_with_label('Reporte de Relaciones');
  btnLogout := gtk_button_new_with_label('Cerrar Sesión');

  // Conectar señales
  g_signal_connect(btnCrearComunidad, 'clicked', G_CALLBACK(@OnCrearComunidadClick), nil);
  g_signal_connect(btnMassiveLoad, 'clicked', G_CALLBACK(@OnMassiveLoadClick), nil);
  g_signal_connect(btnUserReport, 'clicked', G_CALLBACK(@OnUserReportClick), nil);
  g_signal_connect(btnRelationReport, 'clicked', G_CALLBACK(@OnRelationReportClick), nil);
  g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

  // Añadir botones al VBox con expansión y padding
  gtk_box_pack_start(GTK_BOX(vbox), btnCrearComunidad, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnMassiveLoad,    True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnUserReport,     True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnRelationReport, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnLogout,         True, True, 0);

  // Mostrar todos los widgets
  gtk_widget_show_all(rootWindow);

  // Evento para cerrar ventana
  g_signal_connect(rootWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  // Ejecutar loop principal de GTK
  gtk_main;
end;

end.

