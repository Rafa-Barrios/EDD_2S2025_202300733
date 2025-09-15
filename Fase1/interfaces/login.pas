unit login;

interface
    procedure ShowLoginWindow;

implementation

    uses
        gtk2, glib2, gdk2,
        createUser, rootHome, userHome,
        variables, interfaceTools,
        simpleLinkedList, circularLinkedList, jsonTools;

    var
        entryUser, entryPass: PGtkWidget;
        loginWindow: PGtkWidget;

    // Evento para el login.
    procedure OnLoginButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
    var
        userText, passText: PChar;
        isValid: Boolean;
        userData: TUserData;
    begin
        // Obtener el texto
        userText := PChar(gtk_entry_get_text(GTK_ENTRY(entryUser)));
        passText := PChar(gtk_entry_get_text(GTK_ENTRY(entryPass)));

        // Validar el usuario y contraseña
        isValid := LSL_U_ValidateCredentials(userText, passText);

        // Redirección según el resultado de la validación
        if isValid then
        begin
            // Obtener los datos del usuario
            userData := LSL_U_GetUserByEmail(userText);

            // Asignar datos a las variables globales
            current_user_id := userData.id;
            current_user_email := userData.email;
            current_user_name := userData.name;
            current_user_username := userData.username;
            current_user_phone := userData.phone;
            current_user_password := userData.password;

            // 🔹 Reiniciar la lista de contactos en memoria
            CL_ClearList(current_user_contacts);

            // 🔹 Cargar contactos de este usuario desde JSON
            UploadContactsFromJson(
                json_file_contacts,   // ✅ usamos la variable global definida en variables.pas
                current_user_username,
                current_user_contacts
            );

            Writeln('Usuario autenticado: ', current_user_username);

            gtk_widget_destroy(loginWindow);
            ShowUserHomeWindow();
        end
        else if (userText = root_user_email) and (passText = root_user_password) then
        begin
            // Para el usuario root, asignar valores predeterminados o dejar en blanco
            current_user_id := '0';
            current_user_name := 'Root';
            current_user_email := root_user_email;
            current_user_username := 'root';
            current_user_phone := '';
            current_user_password := root_user_password;

            // 🔹 Limpiar contactos para evitar herencias
            CL_ClearList(current_user_contacts);

            gtk_widget_destroy(loginWindow);
            ShowRootHomeWindow();
        end
        else
        begin
            ShowErrorMessage(loginWindow, 'Error de Login',
                'Usuario o contraseña incorrectos, por favor corrobore!');
        end;
    end;

    // Redirección a ventana crear usuario
    procedure OnCreateUserButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(loginWindow);
        ShowCreateUserWindow;
    end;

    // Procedimiento para mostrar la ventana de inicio de sesión
    procedure ShowLoginWindow;
    var
        grid: PGtkWidget;
        lblUser, lblPass: PGtkWidget;
        btnLogin, btnCreateUser: PGtkWidget;
    begin
        // Inicializar la librería GTK
        gtk_init(@argc, @argv);

        // Crear una nueva ventana principal
        loginWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(loginWindow), 'Login');
        gtk_container_set_border_width(GTK_CONTAINER(loginWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(loginWindow), 300, 200);

        // Crear una tabla de 4 filas y 2 columnas
        grid := gtk_table_new(4, 2, False);
        gtk_container_add(GTK_CONTAINER(loginWindow), grid);

        // Etiquetas
        lblUser := gtk_label_new('Correo:');
        lblPass := gtk_label_new('Contraseña:');

        // Campos de entrada
        entryUser := gtk_entry_new;
        entryPass := gtk_entry_new;
        gtk_entry_set_visibility(GTK_ENTRY(entryPass), False);

        // Botón de iniciar sesión
        btnLogin := gtk_button_new_with_label('Iniciar sesión');
        g_signal_connect(btnLogin, 'clicked', G_CALLBACK(@OnLoginButtonClick), nil);

        // Botón de crear usuario
        btnCreateUser := gtk_button_new_with_label('Crear usuario');
        g_signal_connect(btnCreateUser, 'clicked', G_CALLBACK(@OnCreateUserButtonClick), nil);

        // Colocar widgets
        gtk_table_attach_defaults(GTK_TABLE(grid), lblUser,       0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryUser,     1, 2, 0, 1);

        gtk_table_attach_defaults(GTK_TABLE(grid), lblPass,       0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryPass,     1, 2, 1, 2);

        gtk_table_attach_defaults(GTK_TABLE(grid), btnCreateUser, 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnLogin ,     1, 2, 2, 3);

        // Mostrar todo
        gtk_widget_show_all(loginWindow);

        // Evento cerrar ventana
        g_signal_connect(loginWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        // Iniciar el bucle GTK
        gtk_main;
    end;

end.