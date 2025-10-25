unit rootHome;

{$MODE DELPHI}

interface
  procedure ShowRootHomeWindow;
  procedure ShowRootHomeAgain;

implementation

uses
  gtk2, glib2, gdk2,
  login,
  rootCom,
  rootComunidad,
  loginControl, // 👈 NUEVO
  interfaceTools, jsonTools, variables, filesTools, bstTree,
  simpleLinkedList, linkedListOfLists;

var
  rootWindow: PGtkWidget;
  btnCrearComunidad, btnVerMensajesComunidad, btnMassiveLoad,
  btnUserReport, btnRelationReport, btnLoginControl, btnLogout: PGtkWidget; // 👈 NUEVO
  vbox: PGtkWidget;

// --------------------------------------------------------------------
// 1. Redirección a RootComunidad (crear comunidad)
// --------------------------------------------------------------------
procedure OnCrearComunidadClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_hide(rootWindow);
  ShowRootComunidadWindow;
end;

// --------------------------------------------------------------------
// 2. Redirección a RootCom (ver mensajes de comunidad)
// --------------------------------------------------------------------
procedure OnVerMensajesComunidadClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_hide(rootWindow);
  ShowRootComWindow;
end;

// --------------------------------------------------------------------
// 3. Carga masiva de usuarios desde JSON
// --------------------------------------------------------------------
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

// --------------------------------------------------------------------
// 4. Genera los reportes de usuarios y comunidades
// --------------------------------------------------------------------
procedure OnUserReportClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  dotComunidades: string;
begin
  filesTools.GenerateReports('users', 'Root-Reports', LSL_U_GenerateDot());
  dotComunidades := linkedListOfLists.LL_GenerateDot();
  filesTools.GenerateReports('comunidades', 'Root-Reports', dotComunidades);
  BST_GenerateReport;
  ShowSuccessMessage(rootWindow, 'Reportes Generados',
    'Se generaron los reportes de usuarios, comunidades y BST correctamente.');
end;

// --------------------------------------------------------------------
// 5. Genera reportes de relaciones
// --------------------------------------------------------------------
procedure OnRelationReportClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  LSL_U_PrintToConsole();
end;

// --------------------------------------------------------------------
// 6. Mostrar Control de Logueo
// --------------------------------------------------------------------
procedure OnLoginControlClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_hide(rootWindow);
  ShowLoginControlWindow; // 👈 Nueva interfaz
end;

// --------------------------------------------------------------------
// 7. Cerrar sesión
// --------------------------------------------------------------------
procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_hide(rootWindow);
  ShowLoginWindow;
end;

// --------------------------------------------------------------------
// 8. Mostrar ventana Root Home
// --------------------------------------------------------------------
procedure ShowRootHomeWindow;
begin
  gtk_init(@argc, @argv);

  rootWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(rootWindow), 'Root Home');
  gtk_container_set_border_width(GTK_CONTAINER(rootWindow), 20);
  gtk_window_set_default_size(GTK_WINDOW(rootWindow), 400, 380);

  vbox := gtk_vbox_new(False, 15);
  gtk_container_add(GTK_CONTAINER(rootWindow), vbox);

  btnCrearComunidad := gtk_button_new_with_label('Crear Comunidad');
  btnVerMensajesComunidad := gtk_button_new_with_label('Ver Mensajes de Comunidad');
  btnMassiveLoad := gtk_button_new_with_label('Carga Masiva de Usuarios');
  btnUserReport := gtk_button_new_with_label('Reporte de Usuarios y Comunidades');
  btnRelationReport := gtk_button_new_with_label('Reporte de Relaciones');
  btnLoginControl := gtk_button_new_with_label('Control de Logueo'); // 👈 NUEVO
  btnLogout := gtk_button_new_with_label('Cerrar Sesión');

  // Conectar señales
  g_signal_connect(btnCrearComunidad, 'clicked', G_CALLBACK(@OnCrearComunidadClick), nil);
  g_signal_connect(btnVerMensajesComunidad, 'clicked', G_CALLBACK(@OnVerMensajesComunidadClick), nil);
  g_signal_connect(btnMassiveLoad, 'clicked', G_CALLBACK(@OnMassiveLoadClick), nil);
  g_signal_connect(btnUserReport, 'clicked', G_CALLBACK(@OnUserReportClick), nil);
  g_signal_connect(btnRelationReport, 'clicked', G_CALLBACK(@OnRelationReportClick), nil);
  g_signal_connect(btnLoginControl, 'clicked', G_CALLBACK(@OnLoginControlClick), nil); // 👈 NUEVO
  g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

  // Añadir botones
  gtk_box_pack_start(GTK_BOX(vbox), btnCrearComunidad,       True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnMassiveLoad,          True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnUserReport,           True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnRelationReport,       True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnVerMensajesComunidad, True, True, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnLoginControl,         True, True, 0); // 👈 NUEVO
  gtk_box_pack_start(GTK_BOX(vbox), btnLogout,               True, True, 0);

  gtk_widget_show_all(rootWindow);
  g_signal_connect(rootWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
  gtk_main;
end;

// --------------------------------------------------------------------
procedure ShowRootHomeAgain;
begin
  if Assigned(rootWindow) then
    gtk_widget_show_all(rootWindow);
end;

end.
