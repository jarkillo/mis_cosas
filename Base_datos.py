# -*- coding: utf-8 -*-
"""
Created on Fri Oct 13 19:58:00 2023

@author: Jarko
"""

#%%

# El directorio de los clientes de una empresa está organizado en una cadena de texto como la de
# más abajo, donde cada línea contiene la información del nombre, email, teléfono, NIF, y el descuento
# que se le aplica. 

# Las líneas se separan con el carácter de cambio de línea \n y la primera línea
# contiene los nombres de los campos con la información contenida en el directorio. 

# Crear un programa que cree un diccionario de diccionarios para guardar la información.

# "nif;nombre;email;teléfono;descuento\n01234567L;Luis González;
# luisgonzalez@mail.com;656343576;12.5\n71476342J;Macarena Ramírez;
# macarena@mail.com;692839321;8\n63823376M;Juan José Martínez;juanjo@mail.com;
# 664888233;5.2\n98376547F;Carmen Sánchez;carmen@mail.com;667677855;15.7"

    
# Funcion Menu

def menu(): 
    
    salir = False
    
    while not salir:
    
        print ('\n1. Importar usuarios')
        print ('2. Editar usuario')
        print ('3. Buscar usuario')
        print ('4. Borrar usuario')
        print ('5. Borrar DB')
        print ('6. Salir')
        
        opcion = input ('\n Introduce una opción: ')
        
        try:
        
            if int(opcion) == 1:
            
                add_user()
                continue
            
            elif int(opcion) == 2:
            
                numero_usuario = buscar()
                edit_user(numero_usuario)
                continue
            
            
            elif int(opcion) == 3:
            
                buscar()
                continue
            
            elif int(opcion) == 4:
                
                numero_usuario = buscar()
                borrar_user(numero_usuario)
            
            elif int(opcion) == 5:
            
                confirm = input ('Introduce "BORRAR" si está seguro de querer borrar la base de datos: ')
            
                if confirm == "BORRAR":
                
                    base_de_datos.clear()
                    print ('\nBorrando...')
                    print ('\nBase de datos borrada')
                    continue
                            
                else:
                
                    print ("Accion Cancelada")
                    
                    continue
                            
            elif int(opcion) == 6:
            
                salir = True
        
            else:
                
                print ('\n Elige una opción correcta')
           
                continue
            

        except:
            
                print ('\n Elige una opción correcta')
           
                continue
            
        

# Funcion para importar usuarios

def add_user():   

    # Solicitamos el texto de importación

    data_imported = input ('Introduce los datos del usuario con el formato correcto: ')
   
   # Separamos los saltos de linea
    
    listado_usuarios = data_imported.split ('\\n')

    # Separamos los datos
    
    datos_usuarios = [usuario.split(';') for usuario in listado_usuarios]

    # Bucle para escanear los datos y asignarlos a las variables, ignoramos la primera linea 

    for dato in range(len(datos_usuarios)-1):
       
       nif = datos_usuarios[dato+1][0]
       nombre = datos_usuarios[dato+1][1]
       email = datos_usuarios[dato+1][2]
       telefono = datos_usuarios[dato+1][3]
       descuento = datos_usuarios[dato+1][4]
       
       # Actualizamos la base de datos con los nuevos usuarios, dandole la key correspondiente
   
       base_de_datos.update({len(base_de_datos)+1:{'nif':nif, 'nombre': nombre, 'email': email,
                           'telefono': telefono, 'descuento': descuento}})
   
    
    # Mostramos la base de datos (SOLO MODO DEBUG)

    base_de_datos

   
# Funcion para editar usuario   

def edit_user(numero_usuario):       
    
    # Solicitamos la opción del menu
    while True:
        
        opcion = input ('\nIntroduce la opción que quieres editar o x para salir: ')
                         
        try:
    
            if int(opcion) == 1:
                    
                nif = input ('Introduce el nuevo NIF: ')
                base_de_datos[int(numero_usuario)]['nif'] = nif
                    
                
            elif int(opcion) == 2:
                    
                    nombre = input ('Introduce el nuevo Nombre: ')
                    base_de_datos[int(numero_usuario)]['nombre'] = nombre
    
            elif int(opcion) == 3:
                    
                email = input ('Introduce el nuevo Email: ')
                base_de_datos[int(numero_usuario)]['email'] = email
                        
            elif int(opcion) == 4:
                    
                    telefono = input ('Introduce el nuevo Telefono: ')
                    base_de_datos[int(numero_usuario)]['telefono'] = telefono
                    
                    
            if int(opcion) == 5:
                    
                descuento = input ('Introduce el nuevo Descuento: ')
                base_de_datos[int(numero_usuario)]['descuento'] = descuento
                    
        except ValueError:
                    
            if opcion == 'x':

                print ('Hasta pronto')
                break
                   
            else:
                    
                print ('\nIntroduce una opción correcta')
                continue
            
# Funcion borrar usuario  

def borrar_user(numero_usuario):       
    
    # Solicitamos la opción del menu
    
    while True:
        
        opcion = input ('\nEscribe "BORRAR" para confirmar el borrado, esto lo eliminará completamente: ')
                         
        try:
    
            if opcion == "BORRAR":
                    
                print ('\nBorrando...')
                del base_de_datos[int(numero_usuario)]
                print ('\nRegistro Borrado')
                
                break
                
            else:
                
                print ('Operacion Cancelada')
                
                break
        except:
            
            print ('ERROR')
       
# Funcion para buscar

def buscar():   
    
    while True: # Bucle para poder buscar más de una vez
        
        numero_usuario = input ('\nIntroduce la ID del usuario o "x" para volver al menu principal: ')
    
        if numero_usuario == 'x':
            
            break
                  
        else:
        
        # Buscamos el ID en la DB
  
            registro = base_de_datos.get(int(numero_usuario))
            
            if registro == None:
        
                print ('\nNo existe ningun usuario con la ID %s, compruebe el numero o introduzca x para salir' % numero_usuario)
        
                continue
            
            else:
        
                print ('\n------------------------------------------')
                print ('\n1. NIF: ', registro['nif'] )
                print ('2. Nombre: ', registro['nombre'] )
                print ('3. Email: ', registro['email'] )
                print ('4. Telefono: ', registro['telefono'] )
                print ('5. Descuento: ', registro['descuento'] )
                print ('\n------------------------------------------')
                
            return (numero_usuario)
            
  
if "base_de_datos" not in globals():
        
    base_de_datos = {}

menu()


    

                




