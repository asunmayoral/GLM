# **Guía de trabajo con GitHub**

**Cómo trabajar en equipo en los proyectos de la asignatura**

> Lee esta guía entera **antes** de empezar. Está ordenada cronológicamente: los pasos 1 a 5 se hacen una sola vez al principio; el paso 6 lo harás cada día; los pasos 9 y 10 corresponden a la entrega y la revisión.

> A lo largo del curso desarrollaréis **tres proyectos**, cada uno en su propio repositorio y con un **coordinador de equipo distinto**. Los pasos 2, 4 y 5 se repiten en cada proyecto.

---

## **Índice**

1. [Crear tu cuenta en GitHub y comunicar tu nombre de usuario](#1.-crear-tu-cuenta-en-github-y-comunicar-tu-nombre-de-usuario)  
2. [Aceptar la invitación al repositorio de tu equipo](#2-aceptar-la-invitación-al-repositorio-de-tu-equipo)  
3. [Instalar GitHub Desktop y vincularlo con tu cuenta](#3.-instalar-github-desktop-y-vincularlo-con-tu-cuenta)  
4. [Descargar el repositorio a tu ordenador (clonar)](#4.-descargar-el-repositorio-a-tu-ordenador-\(clonar\))  
5. [Crear el proyecto de RStudio y organizar el trabajo del equipo](#5.-crear-el-proyecto-de-rstudio-y-organizar-el-trabajo-del-equipo)  
6. [La rutina de trabajo: *pull* antes de empezar, *push* al terminar](#6.-la-rutina-de-trabajo:-pull-antes-de-empezar,-push-al-terminar)  
7. [Normas de convivencia del equipo](#7.-normas-de-convivencia-del-equipo)  
8. [Qué hacer si algo sale mal](#8-qué-hacer-si-algo-sale-mal)  
9. [Avisar de la entrega abriendo un *issue*](#9.-avisar-de-la-entrega-abriendo-un-issue)  
10. [Consultar los comentarios de la revisión](#10-consultar-los-comentarios-de-la-revisión)

---

## **1\. Crear tu cuenta en GitHub y comunicar tu nombre de usuario**

*Si ya tienes cuenta de GitHub, salta al punto 1.4.*

GitHub es el lugar donde vivirá el proyecto de tu equipo. Todos los miembros trabajaréis sobre los mismos archivos, y cada cambio quedará registrado con el nombre de quien lo hizo y la fecha.

### **1.1. Regístrate**

Entra en [**github.com**](https://github.com) y pulsa **Sign up** (arriba a la derecha). El formulario te pedirá tres cosas:

- **Email**: usa tu correo de la universidad si lo tienes.  
- **Password**: mínimo 8 caracteres. Guárdala donde no la pierdas; la necesitarás durante todo el curso.  
- **Username**: este es el importante. Será tu identificador público y aparecerá junto a cada cambio que hagas en el proyecto.

**Elige el nombre de usuario con cabeza.** Debe permitirme identificarte sin dudas cuando revise el trabajo del equipo. Algo del estilo `martasanchez` o `msanchez-uni` funciona bien. Evita apodos, cifras aleatorias o nombres de fantasía: si tu usuario es `xXdarkcoderXx99`, no sabré quién eres al corregir. No se puede repetir con el de ningún otro usuario del mundo, así que puede que tengas que probar alguna variante.

### **1.2. Verifica tu correo**

GitHub te enviará un código a tu email. Introdúcelo cuando te lo pida. Si no llega en un par de minutos, mira en la carpeta de spam.

### **1.3. Configura la verificación en dos pasos**

GitHub obliga a activar la verificación en dos pasos (2FA). Es un segundo código que confirma que eres tú al iniciar sesión. Puede que te lo pida al registrarte o unos días después; cuando aparezca, no lo pospongas indefinidamente, porque pasado el plazo te bloquea el acceso a la cuenta.

La opción más cómoda es por **aplicación de autenticación**: instala Google Authenticator o Microsoft Authenticator en el móvil, escanea el código QR que te muestre GitHub y listo.

Al terminar, GitHub te dará una lista de **códigos de recuperación**. Descárgalos y guárdalos en un sitio seguro. Son tu única forma de recuperar la cuenta si pierdes el móvil.

### **1.4. Comunícame tu nombre de usuario**

En cuanto tengas la cuenta, envíame tu nombre de usuario de GitHub por el formulario de [Registro Github y Equipos](https://forms.gle/vPAAqR8GVGkcopLd6).

Es imprescindible: sin ese dato no puedo darte acceso al repositorio de tu equipo. Mándame el **nombre de usuario**, y registra también tu correo y tu nombre real, para vincularte correctamente a tu equipo. Si no estás seguro de cuál es, entra en github.com, pulsa tu foto de perfil arriba a la derecha y aparecerá justo debajo de "Signed in as".

---

## **2\. Aceptar la invitación al repositorio de tu equipo**

Una vez me hayas enviado tu nombre de usuario, crearé un espacio de trabajo para tu equipo (lo llamaremos **repositorio**) y te invitaré a él. Un repositorio es sencillamente una carpeta compartida con historial: guarda los archivos del proyecto y, además, el registro de todos los cambios que habéis hecho y quién los hizo.

> **Recibirás una invitación nueva en cada uno de los tres proyectos.** Cada proyecto tiene su propio repositorio, independiente de los anteriores.

### **2.1. Busca la invitación**

Te llegará un correo de GitHub con el asunto *"\[nombre del repositorio\] — invitation to collaborate"*. Contiene un botón verde, **View invitation**.

Si no lo encuentras, mira en spam o en la pestaña "Promociones" si usas Gmail. También puedes verla sin correo: entra en github.com con tu cuenta iniciada y la invitación aparecerá como un aviso en la parte superior de la página principal.

### **2.2. Acepta**

Pulsa **View invitation** y, en la página que se abre, el botón verde **Accept invitation**.

Atención a esto: la invitación **caduca a los 7 días**. Si se te pasa el plazo, avísame y te envío otra; no es grave, pero perderás tiempo.

### **2.3. Comprueba que estás dentro**

Después de aceptar, GitHub te lleva directamente al repositorio. Deberías ver una lista de archivos: es el material de partida del proyecto.

Verifica dos cosas:

- Arriba del todo aparece el nombre del repositorio, con el formato `usuario-del-profesor / nombre-del-repositorio`.  
- Bajo ese nombre hay una fila de pestañas: **Code**, **Issues**, **Pull requests**... Y a la derecha, si eres colaborador, verás también **Settings**. Si ves *Settings*, tienes permiso de escritura, que es lo que necesitas.

### **2.4. Guarda la dirección**

Copia la dirección web del repositorio (lo que aparece en la barra del navegador, del tipo `https://github.com/profesor/proy1-equipo3`) y guárdala. La usarás en el siguiente paso y volverás a ella muchas veces.

Otra forma rápida de llegar en el futuro: entra en github.com y pulsa tu foto de perfil → **Your repositories**. El repositorio del equipo aparecerá en esa lista.

### **2.5. Un vistazo rápido a lo que hay**

Antes de seguir, dedica un minuto a curiosear. Pulsa sobre el archivo **README.md**: es la portada del proyecto y contiene esta misma guía junto con las indicaciones de la tarea.

**No modifiques nada todavía desde la web.** Aunque GitHub permite editar archivos en el navegador, no lo hagas: trabajaremos siempre desde tu ordenador, y mezclar ambas formas es la vía más rápida a los conflictos.

### **Un aviso importante desde ya**

Este repositorio es compartido. Todo lo que subas lo verán tus compañeros y yo, y quedará en el historial permanentemente. No subas ahí contraseñas, datos personales, ni archivos que no formen parte del trabajo.

---

## **3\. Instalar GitHub Desktop y vincularlo con tu cuenta**

Hasta ahora has trabajado en la web. Pero el proyecto no se edita en el navegador: se edita en tu ordenador. **GitHub Desktop** es el programa que conecta ambos mundos, el puente por el que bajarás los cambios de tus compañeros y subirás los tuyos.

Es una aplicación con botones, sin comandos que memorizar. Manejarás básicamente dos.

### **3.1. Descarga el programa**

Ve a [**desktop.github.com**](https://desktop.github.com) y pulsa el botón de descarga. La página detecta tu sistema operativo y te ofrece la versión adecuada (Windows o macOS).

Si usas **Linux**, GitHub Desktop no tiene versión oficial. Avísame y te doy una alternativa.

### **3.2. Instálalo**

- **Windows**: ejecuta el archivo `.exe` descargado. La instalación es automática y el programa se abre solo al terminar.  
- **macOS**: abre el `.zip` y arrastra la aplicación **GitHub Desktop** a tu carpeta *Aplicaciones*. Ábrela desde ahí. Si macOS avisa de que es una aplicación descargada de internet, acepta.

### **3.3. Vincula tu cuenta de GitHub**

Al abrirlo por primera vez verás una pantalla de bienvenida con el botón **Sign in to GitHub.com**. Púlsalo.

Se abrirá tu navegador con la página de GitHub. Inicia sesión con tu usuario y contraseña, introduce el código de verificación en dos pasos si te lo pide, y autoriza la aplicación cuando te lo solicite.

El navegador te preguntará entonces si quieres abrir GitHub Desktop. Acepta. Volverás al programa, ya con tu cuenta conectada.

**Si el navegador no vuelve al programa**: cierra GitHub Desktop del todo, vuelve a abrirlo y repite. Es un fallo ocasional, no has hecho nada mal.

### **3.4. Configura tu identidad**

A continuación te pedirá confirmar tu **nombre** y tu **correo**. Estos datos se adjuntarán a cada cambio que subas, de modo que quede constancia de tu autoría.

Deja los valores que aparecen por defecto, que son los de tu cuenta de GitHub. Solo asegúrate de que el correo coincide con el de tu cuenta: si pones otro distinto, tus aportaciones podrían no aparecer asociadas a tu perfil, y al corregir parecerá que no has trabajado.

Pulsa **Finish**.

### **3.5. Comprueba que ha funcionado**

Verás la pantalla principal del programa, con un mensaje del tipo *"Let's get started\!"* y varias opciones.

Confirma la conexión así: menú **File** → **Options** (en Mac: **GitHub Desktop** → **Settings**) → pestaña **Accounts**. Debe figurar tu nombre de usuario de GitHub. Si aparece, todo correcto.

### **Sobre tokens y contraseñas**

Puede que hayas leído que GitHub exige un "token de acceso" para subir cambios. **Tú no lo necesitas.** GitHub Desktop gestiona la autenticación por su cuenta con la vinculación que acabas de hacer. Si en algún momento un tutorial de internet te pide crear un token, ignóralo: no forma parte de nuestro flujo de trabajo.

---

## **4\. Descargar el repositorio a tu ordenador (clonar)**

Ahora vas a traerte el proyecto del equipo a tu ordenador. En la jerga de Git esto se llama **clonar**: se crea una carpeta local que es una copia del repositorio, pero conectada a él. Cuando bajes o subas cambios, será a través de esa conexión.

Se hace **una vez por repositorio**. Como hay tres proyectos, lo repetirás tres veces a lo largo del curso, **cada uno en su propia carpeta**. No mezcles nunca el contenido de un proyecto con el de otro.

### **4.1. Antes de empezar: decide dónde va a vivir el proyecto**

GitHub Desktop propondrá una ubicación por defecto (normalmente `Documentos\GitHub`). Puedes aceptarla, pero **piensa dónde la pones**, porque después no conviene moverla.

Dos advertencias que evitan la mayoría de los desastres:

- **No la coloques dentro de OneDrive, Dropbox, Google Drive o iCloud.** Estos programas sincronizan por su cuenta y entran en conflicto con Git: acabarás con archivos duplicados, copias con nombres raros y el proyecto corrupto. Es el error más frecuente y el más difícil de arreglar. Escoge una ruta fuera de cualquier carpeta sincronizada.  
- **Evita rutas con acentos, eñes o espacios raros.** R y Git a veces tropiezan con ellas. `C:\Proyectos\` es mejor que `C:\Documentos de María\Análisis nº2\`.

### **4.2. Clona el repositorio**

En GitHub Desktop:

1. Menú **File** → **Clone repository**.  
2. Se abre una ventana con varias pestañas. Quédate en **GitHub.com**.  
3. Aparecerá la lista de repositorios a los que tienes acceso. Localiza el de tu equipo y selecciónalo.  
   - *Si no aparece*: comprueba que aceptaste la invitación (paso 2). También puedes pulsar el icono de refrescar, o usar la pestaña **URL** y pegar ahí la dirección que guardaste.  
4. En **Local path** verás la carpeta de destino. Cámbiala si quieres con el botón **Choose...**, respetando las advertencias de 4.1.  
5. Pulsa **Clone**.

La descarga tarda unos segundos. Al terminar, GitHub Desktop te lleva a la pantalla principal del repositorio.

### **4.3. Si te pregunta cómo vas a usarlo**

Puede aparecer una pregunta: *"How are you planning to use this fork?"* — ignórala, no aplica a nuestro caso. Si te sale, elige **For my own purposes** y continúa.

### **4.4. Comprueba que tienes los archivos**

En GitHub Desktop, pulsa el botón **Show in Explorer** (Windows) o **Show in Finder** (Mac), en el centro de la pantalla. Se abrirá la carpeta local.

Deberías ver los mismos archivos que viste en la web. Si es así, ya lo tienes.

Verás también una carpeta oculta llamada `.git` (o no la verás, según la configuración de tu sistema). Es el cerebro de Git: guarda todo el historial. **No la abras, no la muevas y no la borres.** Si desaparece, la carpeta deja de estar conectada con GitHub.

### **4.5. Entiende dónde estás**

Este punto es la clave de todo lo que viene después. Ahora existen **dos copias** del proyecto:

- La de **tu ordenador**, en la carpeta que acabas de crear. Es donde trabajarás.  
- La de **GitHub**, en la web. Es la copia común del equipo.

**No se sincronizan solas.** Guardar un archivo en tu ordenador no lo sube a GitHub. Que un compañero suba algo no lo hace aparecer en tu carpeta. Esa conexión la haces tú a mano, con dos botones, y es exactamente de lo que trata el paso 6\.

Mientras tanto, recuerda: **trabaja siempre dentro de esa carpeta**. No copies los archivos a otro sitio para editarlos ahí, ni te crees una carpeta paralela "para pruebas". Todo ocurre dentro de la carpeta clonada.

---

## **5\. Crear el proyecto de RStudio y organizar el trabajo del equipo**

En el repositorio que has clonado están los materiales de partida, pero **falta el archivo que convierte esa carpeta en un proyecto de RStudio**. Lo vais a crear vosotros. Es rápido y solo se hace una vez por repositorio.

Como hay tres proyectos, este paso se repetirá tres veces a lo largo del curso, y lo ejecutará cada vez el **coordinador de turno**.

### **5.1. Quién crea el proyecto: solo una persona**

Esto es importante. **El proyecto de RStudio lo crea únicamente el coordinador del equipo.** El resto no crea nada: se lo descargará ya hecho.

Si cada miembro crea el suyo, acabaréis con tres o cuatro archivos de proyecto distintos en la misma carpeta, y nadie sabrá cuál abrir. Poneos de acuerdo antes: **una persona lo crea, avisa al grupo, y los demás esperan**.

### **5.2. Instrucciones para el coordinador**

1. Abre RStudio.  
2. Menú **File** → **New Project...**  
3. En la ventana que aparece, elige **Existing Directory**. Es la opción correcta: la carpeta ya existe y tiene contenido, no queremos crear una nueva.  
4. Pulsa **Browse...** y localiza la carpeta que clonaste con GitHub Desktop (paso 4). Selecciónala y confirma.  
5. Pulsa **Create Project**.

RStudio se reiniciará con el proyecto abierto. En la carpeta habrá aparecido un archivo con extensión **`.Rproj`**, que lleva el nombre de la carpeta.

Verás también, arriba a la derecha de RStudio, el nombre del proyecto. Y en el panel superior derecho, una pestaña nueva llamada **Git**: es la señal de que RStudio ha detectado que esta carpeta está conectada a GitHub.

RStudio habrá creado además un archivo **`.gitignore`**. Es una lista de archivos que no deben subirse (`.Rhistory`, `.RData`, la carpeta `.Rproj.user`: material temporal que no interesa a nadie). No lo borres ni lo modifiques.

6. **Súbelo al repositorio ahora mismo.** Abre GitHub Desktop, escribe en el recuadro de abajo a la izquierda un mensaje como `Añado el proyecto de RStudio`, pulsa **Commit to main** y después **Push origin**. El paso 6 explica esta rutina en detalle; por ahora, hazlo y avisa al equipo de que ya está subido.

### **5.3. Instrucciones para el resto del equipo**

Cuando el coordinador os avise:

1. Abre **GitHub Desktop** y pulsa **Fetch origin** (arriba a la derecha). Si aparece **Pull origin**, púlsalo también.  
2. Ve a tu carpeta local. Ahí estará ya el archivo `.Rproj`.  
3. **Doble clic sobre él.** RStudio se abrirá con el proyecto cargado.

Ya está. No crees tú ningún proyecto.

### **5.4. A partir de ahora, abre siempre por el `.Rproj`**

Cada vez que te sientes a trabajar, abre el proyecto haciendo doble clic en el archivo `.Rproj`. No abras los `.qmd` sueltos desde RStudio.

La razón es práctica: al abrir por el `.Rproj`, R entiende que la carpeta del proyecto es su punto de partida, y las rutas funcionan igual en el ordenador de todos. Si lo abres de otro modo, tu código funcionará en tu máquina y fallará en la de tus compañeros.

Por lo mismo: **nunca escribas `setwd()`** en tu código.

### **5.5. Qué hay en la carpeta (y qué tendréis que añadir vosotros)**

El repositorio arranca casi vacío. Eso es intencionado: casi todo lo que contenga al final lo habréis puesto vosotros.

Al clonar encontrarás:

- **`README.md`** — Esta guía. Es la portada del repositorio: cuando entras por la web, se muestra automáticamente debajo de la lista de archivos. Vuelve a él siempre que dudes.  
- **`plantilla_informe.qmd`** — El molde del informe final, correspondiente a este proyecto. Respeta su estructura; ahí volcaréis lo que decidáis incluir.  
- **`bitacora.md`** — El registro semanal del trabajo del equipo. Lo explico en 5.6.

**No hay carpeta de datos.** Los datos están publicados en otro repositorio de GitHub y los leeréis directamente desde su URL, sin descargarlos. En el enunciado tenéis la dirección.

Para leer un CSV alojado en GitHub necesitas la URL del archivo **en bruto**: entra al archivo en la web, pulsa el botón **Raw**, y copia la dirección que aparece en el navegador (contiene `raw.githubusercontent.com`). Esa es la que va en tu código:

```
datos <- read.csv("https://raw.githubusercontent.com/.../archivo.csv")
```

Si usas la URL normal de GitHub en lugar de la Raw, R te devolverá una página web en vez de datos y no entenderás el error.

### **5.6. La bitácora: el archivo que más me importa**

`bitacora.md` es donde el **coordinador del equipo** deja constancia, **cada semana**, del reparto de tareas y de cómo va cada una.

No es burocracia. Cumple tres funciones: permite al equipo saber quién hace qué sin preguntarlo por chat, me permite a mí seguir el avance sin interrumpiros, y sobre todo **me dice a quién corresponde cada archivo de código** cuando corrijo. Un análisis excelente en un archivo que la bitácora no menciona es un análisis del que no sé quién es el autor.

**Quién la escribe:** solo el coordinador. Es un archivo compartido y, si lo tocáis varios a la vez, aparecerán conflictos.

**Cuándo:** una entrada por semana, aunque la semana haya sido floja. Una entrada que diga "sin avances por exámenes" es información útil; el silencio no.

**Formato sugerido.** Copia este bloque al principio del archivo cada semana, dejando debajo las semanas anteriores (lo más reciente arriba):

```
## Semana 3 — 5 al 11 de marzo

**Estado general:** en plazo / con retraso / bloqueados

| Tarea | Responsable | Archivo | Estado |
|---|---|---|---|
| Cuestión 1: descriptivos | Marta | analisis_c1_marta.qmd | Terminada |
| Cuestión 2: contraste de medias | Javier | analisis_c2_javier.qmd | En curso |
| Cuestión 3: regresión | Lucía | analisis_c3_lucia.qmd | Sin empezar |
| Redacción del informe | Marta | informe.qmd | Sin empezar |

**Reunión semanal:** celebrada el 8 de marzo. Puntos tratados: ...

**Acuerdos de la semana:**
- Decidimos excluir los casos con edad no declarada.

**Dificultades / dudas para el profesor:**
- No sabemos si aplicar test paramétrico con n=18.
```

Cambia lo que necesites, pero **conserva la tabla**: es lo que hace que la información sea consultable de un vistazo.

**Cómo se edita:** es un archivo de texto normal. Puedes abrirlo desde RStudio como cualquier otro y escribir en él. Los símbolos `##`, `|` y `-` son marcas de formato de Markdown; en GitHub se verán como títulos y tablas bien presentadas.

### **5.7. Un archivo de código por persona y por tarea**

Todo lo demás —los archivos de análisis, el informe, la presentación— lo creáis vosotros dentro de esta misma carpeta.

La regla es: **partid el trabajo en tareas, y que cada tarea tenga su propio archivo con un único responsable.**

Nombra tus archivos de modo que se identifique **la tarea y la persona**:

```
analisis_c1_marta.qmd
analisis_c2_javier.qmd
```

Y ese nombre exacto es el que debe figurar en la columna *Archivo* de la bitácora. Así, cuando yo abra el repositorio, sabré sin preguntar qué archivo responde a qué cuestión y quién lo firma.

Dos consecuencias prácticas:

- **Solo tú editas tu archivo.** Los demás lo leen, lo comentan, lo aprovechan, pero no escriben en él. Si alguien ve un error en el archivo de otro, se lo dice; no lo corrige por su cuenta.  
- **Si una tarea la lleváis entre dos**, partidla en dos subtareas con dos archivos. Es preferible a compartir uno.

El motivo es técnico y vale la pena entenderlo: Git combina sin dificultad los cambios de varias personas **cuando cada una trabaja en un archivo distinto**. Cuando dos editan el mismo archivo a la vez, se produce un **conflicto**, que hay que resolver a mano y es la principal causa de atascos y de trabajo perdido en equipos que empiezan. Con un archivo por persona, esto prácticamente no ocurre.

Los únicos archivos compartidos serán `bitacora.md` e `informe.qmd`, y ambos tienen un único responsable designado. Por eso funcionan.

> ### **⚠️ Un aviso para evitar el malentendido más grave de esta guía**

> Que cada uno tenga su archivo es una medida **técnica**, para que Git no os dé problemas. **No es un reparto intelectual del proyecto.**

> **Todos tenéis que entender el proyecto entero**: todas las cuestiones, todos los análisis, todas las decisiones. No solo la parte que os haya tocado escribir. El examen final versa sobre el proyecto completo, no sobre vuestra tarea individual, y en la entrevista de revisión puedo preguntar a cualquiera por cualquier parte.

> **Separad los archivos. No separéis el conocimiento.** El mecanismo para conseguirlo es la reunión semanal (norma 7.2).

### **5.8. Cómo escribir tu archivo de análisis**

Un `.qmd` (Quarto) alterna texto normal y bloques de código. Un bloque se abre y se cierra así:

````
```{r}
media <- mean(datos$edad)
media
```
````

Entre bloques escribes en lenguaje corriente qué haces y qué concluyes.

**Escribe pensando en tus compañeros y en el profesor.** Antes de cada análisis, una o dos frases: qué pregunta abordas, por qué ese método, qué observas en el resultado. No es adorno. Es lo que permite que el equipo entienda tu parte sin descifrar tu código, y lo que hace posible decidir después qué entra en el informe.

Para crear tu archivo: en RStudio, **File** → **New File** → **Quarto Document**, y guárdalo con el nombre acordado dentro de la carpeta del proyecto.

Para ver cómo queda, pulsa **Render** en la barra superior. Hazlo con frecuencia: si el documento no renderiza es que hay un error, y localizarlo es mucho más fácil cuando llevas diez líneas escritas que cuando llevas trescientas.

### **5.9. El informe final**

Cuando el equipo haya decidido qué resultados merecen entrar, **una sola persona** —la responsable del informe— copia `plantilla_informe.qmd` a `informe.qmd` y va integrando ahí el material seleccionado.

Insisto en lo de "una sola persona": este es el único archivo realmente compartido, y donde sí podrían surgir conflictos. Que lo edite una, mientras el resto revisa y propone cambios por el canal que uséis.

Al terminar, **Render** para generar `informe.html`. **Ese HTML también hay que subirlo** al repositorio: es la versión que consultaré yo.

### **5.10. La presentación**

El formato es libre (PowerPoint, Google Slides, Canva, Quarto...), pero debe quedar accesible desde el repositorio:

- Si es un **archivo** (`.pptx`, `.pdf`): colócalo directamente en la carpeta del proyecto. Que no pase de 100 MB.  
- Si está **en la nube**: crea un archivo de texto llamado **`presentacion.md`** en la carpeta, con el enlace y una línea de contexto:

```
# Presentación del equipo 3
Enlace: https://docs.google.com/presentation/d/...
Formato: Google Slides
```

Si eliges esta vía, **comprueba que el enlace es accesible para cualquiera que tenga la URL**. Un enlace restringido equivale a no haber entregado.

### **5.11. Lo que no debe subirse**

Además de los archivos temporales que ya filtra el `.gitignore`: nada de datos personales, contraseñas ni material ajeno al trabajo. Todo lo que subas queda en el historial de forma permanente, aunque después lo borres.

---

## **6\. La rutina de trabajo: *pull* antes de empezar, *push* al terminar**

Este es el paso que usarás todos los días. Los anteriores se hacen una vez; este, cada vez que te sientes a trabajar.

### **6.1. La idea, en una frase**

Recuerda lo que vimos en 4.5: existen dos copias del proyecto, la tuya y la de GitHub, y **no se sincronizan solas**. Tu rutina consiste en sincronizarlas a mano, al empezar y al acabar:

> **Antes de trabajar, *pull*. Después de trabajar, *push*.**

*Pull* trae a tu ordenador lo que hayan subido tus compañeros. *Push* sube lo tuyo. Si interiorizas solo esto, el resto del curso irá bien.

### **6.2. Al empezar la sesión: traer los cambios (*pull*)**

Antes de abrir RStudio, antes de tocar nada:

1. Abre **GitHub Desktop**.  
2. Comprueba arriba a la izquierda que estás en el repositorio correcto (si tienes varios proyectos clonados, el desplegable **Current repository** te deja cambiar).  
3. Pulsa el botón de arriba a la derecha. Dirá **Fetch origin**: púlsalo. GitHub Desktop consultará si hay novedades.  
4. Si aparecen cambios, el botón pasará a decir **Pull origin** con un número. Púlsalo. Tus archivos locales se actualizarán.

Si el botón sigue diciendo *Fetch origin* sin número, es que no hay nada nuevo. Perfecto, puedes empezar.

**Hazlo siempre, aunque creas que nadie ha tocado nada.** Cuesta cinco segundos y evita el escenario más molesto: trabajar sobre una versión antigua y descubrirlo al intentar subir.

Ahora sí, abre el `.Rproj` y trabaja.

### **6.3. Al terminar: subir los cambios (*push*)**

Cuando acabes la sesión —o antes, si has completado algo con sentido propio:

1. **Guarda todos los archivos en RStudio** (Ctrl+S / Cmd+S en cada pestaña abierta). GitHub Desktop solo ve lo que está guardado en disco.  
2. Vuelve a **GitHub Desktop**. En la columna izquierda verás la lista de archivos que has modificado, y en el centro, en verde y rojo, exactamente qué líneas has cambiado. Échale un vistazo: es una buena forma de detectar que has tocado algo sin querer.  
3. Asegúrate de que están marcadas las casillas de los archivos que quieres subir. Por defecto lo están todos.  
4. Abajo a la izquierda hay dos recuadros. En el primero, **escribe un mensaje** que describa lo que has hecho. Es obligatorio.  
5. Pulsa el botón azul **Commit to main**.  
6. Arriba a la derecha aparecerá **Push origin**. **Púlsalo.**

**El paso 6 es el que se olvida.** Hacer *commit* solo guarda el cambio en tu ordenador; hasta que no pulsas *Push*, tus compañeros y yo no vemos nada. Si al terminar el botón sigue diciendo *Push origin* con un número, no has subido.

### **6.4. Sobre los mensajes de *commit***

Escribe algo que sirva para entender el historial dentro de un mes:

|  |  |
| :---- | :---- |
| ✅ Bien | `Cuestión 2: contraste de medias y gráfico de cajas` |
| ✅ Bien | `Corrijo el filtrado de valores perdidos` |
| ❌ Mal | `cambios`, `.`, `asdf`, `subida` |

No es un capricho. Cuando yo revise el trabajo, el historial de commits me muestra quién aportó qué y cuándo. Y a vosotros os permite volver atrás si algo se rompe.

### **6.5. Con qué frecuencia subir**

Haz *commit* \+ *push* **cada vez que completes algo con sentido**: una cuestión resuelta, un gráfico terminado, una sección escrita. Como referencia, al menos una vez por sesión de trabajo.

Ni un commit por cada línea, ni uno solo al final de tres semanas. El segundo error es el grave: mientras no subas, tu trabajo existe únicamente en tu ordenador. Si se te estropea el portátil, se pierde entero.

### **6.6. Un mapa de la sesión completa**

```
Abrir GitHub Desktop  →  Fetch / Pull origin
        ↓
Abrir el .Rproj  →  trabajar en RStudio  →  guardar (Ctrl+S)
        ↓
GitHub Desktop  →  mensaje  →  Commit to main  →  Push origin
```

### **6.7. Comprueba que ha llegado**

Las primeras veces, verifícalo: entra en el repositorio en github.com y mira si aparece tu cambio. En la lista de archivos, cada uno muestra a la derecha el mensaje del último commit que lo tocó y hace cuánto fue.

Cuando le hayas cogido el aire, ya no hará falta.

### **6.8. Si estáis trabajando a la vez**

A veces coincidiréis dos en la misma tarde. En ese caso:

- Haz **pull** también **antes de cada push**, no solo al empezar. Si un compañero ha subido algo mientras tú trabajabas, GitHub Desktop te avisará de que debes traerlo antes de subir lo tuyo.  
- Mientras cada uno esté en su archivo, esa mezcla es automática y no notarás nada.  
- Si el mensaje que aparece habla de un **conflicto**, no toques nada y ve al paso 8\.

---

## **7\. Normas de convivencia del equipo**

Git resuelve bien los problemas técnicos. Los que no resuelve son los de coordinación: dos personas escribiendo lo mismo, alguien que descubre en la última semana que su parte dependía de otra que nadie hizo, un archivo que aparece modificado y nadie sabe por quién.

Estas ocho normas cubren casi todo lo que suele salir mal.

### **7.1. Cada archivo tiene un dueño, y solo él escribe en él**

Ya está dicho, pero es la norma central y conviene repetirla: **no edites el archivo de otro**. Ni para corregir una errata, ni para "arreglarle" un gráfico, ni aunque estés seguro de que hay un error.

Si ves algo mejorable en el archivo de un compañero, díselo por el canal del equipo y que lo cambie él. Cuesta cinco minutos más y evita que dos personas modifiquen lo mismo a la vez, que es exactamente el escenario que genera conflictos.

Vale también al revés: **tu archivo es tu responsabilidad**. Si algo en él no funciona, no esperes a que alguien lo arregle.

> **Ojo:** "no edites el archivo de otro" no significa "no leas el archivo de otro". Al contrario: leedlos, preguntad, comentad. Lo que no se toca es el teclado ajeno; el contenido es asunto de todos.

### **7.2. Reunión semanal: contaos qué habéis hecho y por qué**

Reservad **una reunión corta cada semana** —media hora basta— en la que cada uno explique al resto:

- Qué ha hecho desde la última reunión.  
- **Por qué lo ha hecho así**: qué método ha elegido y qué descartó.  
- Qué ha encontrado y qué le preocupa.

No es un parte de trabajo. Es el mecanismo por el que el equipo entiende su propio proyecto. Explicar en voz alta lo que has hecho es además la forma más rápida de descubrir los agujeros: si no consigues justificar por qué usaste ese contraste, aún estás a tiempo de revisarlo.

Que el coordinador recoja lo esencial en la bitácora de esa semana.

**Por qué insisto:** el examen final es sobre el proyecto completo. Si llegáis a él sabiendo solo vuestra parte, sabréis un tercio de lo que se os va a preguntar. Y esa comprensión no se improvisa la última semana: se construye con estas reuniones.

**En cualquier caso, celebrad una reunión de puesta en común antes de la entrega**, aunque hayáis fallado alguna semana. Es requisito para entregar (ver 9.1).

### **7.3. La bitácora la escribe solo el coordinador**

`bitacora.md` e `informe.qmd` son los únicos archivos compartidos del repositorio, y por eso tienen un responsable único. Si necesitas que conste algo en la bitácora, pídeselo al coordinador.

### **7.4. Antes de crear cualquier archivo, dilo**

Un archivo nuevo que aparece sin avisar genera desconcierto: nadie sabe si es parte del trabajo, una prueba, o algo subido por error.

Antes de crear un archivo de análisis, anúncialo al equipo con el nombre exacto que vas a usar, y que el coordinador lo apunte en la bitácora. Vale también para archivos auxiliares: si generas un `.csv` con datos transformados o guardas una imagen, que el equipo lo sepa.

### **7.5. Avisa cuando subas algo relevante**

Un *push* es silencioso: nadie recibe una notificación. Si has subido algo que afecta a los demás —un resultado del que depende otra tarea, un cambio en el informe, una decisión metodológica—, escríbelo en el chat del equipo.

Bastan dos líneas: *"Subida la cuestión 2\. He excluido los casos sin edad declarada, ojo si alguien usa esa variable."*

### **7.6. Sincroniza también al empezar, aunque solo vayas a leer**

Ya está en el paso 6, pero es la norma que más se incumple: **pull al empezar, push al terminar**. Siempre.

El fallo típico no es olvidar el pull; es olvidar el **push** al terminar. Antes de cerrar el ordenador, mira el botón de arriba a la derecha de GitHub Desktop. Si dice *Push origin* con un número, tu trabajo no está subido.

### **7.7. No dejéis el informe para el final**

El error más común, y el más caro. El informe no es "juntar los archivos al terminar": exige seleccionar qué resultados entran, unificar el estilo y comprobar que todo renderiza junto. Eso lleva más tiempo del que parece.

Fijad una fecha interna, al menos una semana antes de la entrega, en la que el análisis se da por cerrado y empieza la redacción. Anotadla en la bitácora.

Y no dejéis para el último día ni el **Render** del informe ni la **subida del `informe.html`**: si algo falla al renderizar, querrás margen para arreglarlo.

### **7.8. Si te atascas, que se note**

Si llevas dos días bloqueado con algo, dilo. Primero en el chat del equipo, por si alguien lo ha resuelto ya.

Si nadie sabe resolverlo, díselo al **coordinador del equipo**, que hará **dos cosas**:

1. **Anotarlo en la bitácora**, en la sección *Dificultades / dudas para el profesor*, para que quede registrado.  
2. **Avisarme directamente** por **Google Chat** o por **email (asun.mayoral@umh.es)**.

Los dos pasos, no uno. La bitácora deja constancia, pero yo no la consulto a diario: sin el aviso, vuestra duda puede quedar ahí una semana. Y el aviso sin la anotación no deja rastro de cuándo surgió el problema.

En el mensaje, indicad el equipo, el archivo afectado y qué habéis probado ya. Con eso puedo responder mucho más rápido.

Un bloqueo comunicado a tiempo es un problema pequeño. El mismo bloqueo descubierto en la última semana es un problema del equipo entero.

### **Resumen para tener a mano**

| Norma | En una línea |
| :---- | :---- |
| Un archivo, un dueño | No edites el archivo de otro (pero sí léelo) |
| Reunión semanal | Todos han de entender el proyecto **completo** |
| Bitácora e informe | Solo el coordinador / el responsable designado |
| Archivos nuevos | Anúncialos antes de crearlos |
| Después del push | Avisa por el chat si afecta a otros |
| Cada sesión | Pull al empezar, push al terminar |
| El informe | Fecha interna una semana antes |
| Los atascos | Bitácora **y** aviso al profesor |

---

## **8\. Qué hacer si algo sale mal**

Casi todos los problemas de GitHub se repiten siempre los mismos. Aquí están, con su solución.

> **Regla general: si algo falla, no improvises.** No borres la carpeta, no clones otra vez encima, no copies archivos de un sitio a otro. Casi nunca se pierde nada de verdad en Git, pero sí se pierde intentando arreglarlo a lo bruto. Lee, y si no encaja con ningún caso de aquí, pregunta.

### **8.1. "He hecho commit pero mis compañeros no ven nada"**

Te falta el *push*. Commit guarda en tu ordenador; push sube.

Mira arriba a la derecha en GitHub Desktop: si dice **Push origin** con un número, púlsalo.

### **8.2. "Al pulsar Push me dice que antes debo hacer pull"**

Normal: alguien ha subido algo mientras trabajabas.

Pulsa **Pull origin**. Si cada uno está en su archivo, se combinará solo y podrás hacer *push* a continuación. Si aparece la palabra *conflict*, ve al 8.5.

### **8.3. "GitHub Desktop no detecta mis cambios"**

Tres causas posibles, en este orden:

1. **No has guardado en RStudio.** Ctrl+S / Cmd+S en cada pestaña. Si la pestaña tiene el nombre en rojo o con asterisco, no está guardada.  
2. **Estás en el repositorio equivocado.** Con tres proyectos clonados es fácil. Comprueba **Current repository** arriba a la izquierda.  
3. **Has trabajado fuera de la carpeta clonada.** Si guardaste el archivo en el escritorio o en Descargas, Git no lo ve. Muévelo dentro de la carpeta del proyecto.

### **8.4. "Mi archivo no aparece en GitHub, pero los demás sí"**

Probablemente esté en la lista de archivos ignorados (`.gitignore`), o lo hayas desmarcado sin querer al hacer commit.

Revisa en GitHub Desktop la columna izquierda: si tu archivo aparece ahí sin la casilla marcada, márcala y haz commit. Si no aparece en absoluto y sí existe en la carpeta, avísame.

### **8.5. Conflictos: qué son y qué hacer**

Un **conflicto** ocurre cuando tú y otra persona habéis modificado **las mismas líneas del mismo archivo** y Git no puede decidir cuál vale.

GitHub Desktop te lo dirá con claridad, mencionando *conflicted files*.

**Qué NO hacer:** no cierres el programa esperando que se arregle, no borres la carpeta, no clones de nuevo.

**Qué hacer, en orden:**

1. **Avisa a la persona con la que has coincidido.** Un conflicto es siempre entre dos personas concretas; resolvedlo hablando, no cada uno por su cuenta.  
2. GitHub Desktop ofrece dos botones rápidos si el archivo es de uno solo: descartar tu versión o descartar la del otro. **Úsalos solo si tenéis clarísimo cuál conserváis**, porque la descartada se pierde.  
3. Si hay trabajo válido en las dos versiones, abre el archivo en RStudio. Encontrarás algo así:

```
<<<<<<< HEAD
media <- mean(datos$edad, na.rm = TRUE)
=======
media <- mean(datos$edad)
>>>>>>> origin/main
```

Arriba está tu versión, abajo la del otro. Decidid entre los dos qué queda, **borrad las tres líneas de marcas** (`<<<<<<<`, `=======`, `>>>>>>>`), guardad, y volved a GitHub Desktop: el botón **Continue merge** ya estará disponible. Después, *push*.

4. Si os agobia, **paradlo y escribidme**. Prefiero resolverlo con vosotros que encontrarme el archivo destrozado.

**Y después, id a la causa:** si hubo conflicto, es que dos personas estaban editando el mismo archivo. Revisad el reparto de la bitácora.

### **8.6. "He borrado o roto un archivo sin querer"**

Se recupera. Git guarda todas las versiones anteriores.

- Si **aún no has hecho commit**: en GitHub Desktop, clic derecho sobre el archivo en la lista de cambios → **Discard changes**. Vuelve a como estaba en el último commit.  
- Si **ya hiciste commit y push**: escríbeme. Se recupera desde el historial, pero es más delicado y prefiero guiarte.

Este es el motivo por el que conviene hacer commits frecuentes: cuanto más reciente sea el último punto de guardado, menos se pierde.

### **8.7. "Me pide usuario y contraseña / habla de un token"**

No debería pasar usando GitHub Desktop. Si ocurre, es que estás intentando subir desde otro sitio (la pestaña Git de RStudio, o una terminal).

Cierra eso y hazlo desde GitHub Desktop. **No crees ningún token.**

Si GitHub Desktop mismo pide reautenticación, ve a **File** → **Options** → **Accounts** y vuelve a iniciar sesión.

### **8.8. "He movido la carpeta del proyecto y ya no funciona"**

GitHub Desktop la busca donde estaba. Ve a **Repository** → **Remove** (esto no borra archivos, solo la referencia), y luego **File** → **Add local repository**, señalando la nueva ubicación.

Y recuerda 4.1: no la metas en OneDrive, Drive ni Dropbox.

### **8.9. "El informe no renderiza"**

Esto es un problema de R, no de GitHub. Un par de cosas antes de pedir ayuda:

- Lee el mensaje de error: suele indicar la línea.  
- Comprueba que has hecho *pull*, por si el fallo está en un archivo que ha cambiado.  
- Prueba a renderizar tu archivo de análisis por separado, para aislar dónde está el error.

Si no lo localizas, escríbeme **copiando el mensaje de error completo**.

### **Cuándo escribirme**

Sin dudarlo si aparece la palabra *conflict* y no lo tenéis claro, si creéis que se ha perdido trabajo, o si llevas más de media hora atascado con lo mismo.

En el mensaje incluye: equipo, qué intentabas hacer, qué dice exactamente el mensaje de error (una captura vale) y qué has probado ya.

---

## **9\. Avisar de la entrega abriendo un *issue***

Cuando el trabajo esté terminado, hay que comunicármelo formalmente. No basta con que los archivos estén en el repositorio: yo no reviso repositorios al azar, reviso los que se han declarado entregados.

Para eso usaremos un **issue**. Un issue es un hilo de conversación dentro del repositorio, con fecha, autor y estado (abierto o cerrado). Es donde vosotros anunciáis la entrega y donde yo cierro la revisión.

**Quién lo abre: solo el coordinador del equipo.** Uno por equipo, no uno por persona.

### **9.1. Antes de abrir el issue: la lista de comprobación**

Repasadla entera. Un issue abierto con material incompleto retrasa la corrección de todos.

- [ ] Todos los archivos de análisis están subidos y renderizan sin error.  
- [ ] `informe.qmd` está terminado.  
- [ ] `informe.html` está **generado y subido**. Este es el que más se olvida: renderizar en vuestro ordenador no lo sube a GitHub.  
- [ ] La presentación está en el repositorio (como archivo) o enlazada en `presentacion.md`.  
- [ ] Si es un enlace, **lo habéis abierto desde una ventana de incógnito** para confirmar que es accesible sin permisos.  
- [ ] `bitacora.md` está al día, con todas las semanas y el reparto de tareas.  
- [ ] Todo el mundo ha hecho **push**. Que cada miembro lo confirme: si alguien tiene trabajo sin subir, no está entregado.  
- [ ] **Habéis celebrado la reunión final de puesta en común**, en la que cada uno ha explicado su parte al resto y todos habéis podido preguntar. Si alguien no sabría defender el informe completo, esta reunión no está hecha.

> La última no la puedo verificar mirando el repositorio, pero se nota inmediatamente en la entrevista.

Para verificar las demás con rigor: entrad en el repositorio en github.com y mirad la lista de archivos. Es la forma segura de saber qué hay realmente entregado.

### **9.2. Cómo abrir el issue**

1. Entra en el repositorio de tu equipo en **github.com**.  
2. En la fila de pestañas de arriba, pulsa **Issues**.  
3. Botón verde **New issue**.  
4. Rellena el título y el cuerpo (plantilla abajo).  
5. Pulsa **Submit new issue**.

Yo recibo aviso automáticamente. No hace falta que me escribáis además por correo.

### **9.3. Qué escribir**

Por supuesto, tienes que adaptar el número del proyecto, el nombre del equipo, el título del informe y la presentación, el nombre del repositorio
y todos los datos relativos a los miembros del equipo.

**Título** — con este formato exacto, para que localice las entregas de un vistazo:

```
Entrega Proyecto X — Equipo XXXX
```

**Cuerpo** — copia y adapta esto:

```
## Entrega Proyecto X — Equipo XXXX

**Coordinador:** Marta Sánchez (@martasanchez) 

**Miembros y tareas:** (actualizados con sus nombres)
- @martasanchez — Cuestión 1 (analisis_c1_marta.qmd) y redacción del informe
- @javierlopez — Cuestión 2 (analisis_c2_javier.qmd)
- @luciaperez — Cuestión 3 (analisis_c3_lucia.qmd)

**Informe:** informe.qmd / informe.html 
**Visualización web del informe:** https://htmlpreview.github.io/?https://github.com/asunmayoral/repo/blob/main/informe.html
**Presentación:** presentacion.md (enlace a Google Slides)

**Comentarios para el profesor:**
Los comentarios que procedan y sean pertinentes para la corrección.
```

Dos detalles:

- **Escribe los nombres de usuario con `@` delante.** GitHub los convierte en enlaces y notifica a esas personas, de modo que todo el equipo sigue el hilo.  
- El apartado de comentarios no es relleno. Es donde justificáis decisiones que de otro modo yo interpretaría como errores.
- Para que yo visualice correctamente el informe, necesito que actualices, en la url de visualización, el nombre del repositorio (repo) y el del informe (informe.html).

### **9.4. Después de abrirlo: no toquéis nada**

A partir de ese momento, **dejad de subir cambios al repositorio**.

Si seguís haciendo push mientras yo reviso, estaré leyendo una versión y vosotros modificando otra. Mis comentarios apuntarán a líneas que ya no existen y la corrección se vuelve un lío.

Si detectáis un error grave después de entregar, **no lo corrijáis por vuestra cuenta**: escribidlo como comentario en el mismo issue y yo os digo si procede subir la corrección.

### **9.5. Qué pasa a continuación**

1. Recibo el aviso y reviso el repositorio.  
2. Dejo comentarios sobre líneas concretas del informe y del código, allí donde toquen.  
3. Escribo un comentario de cierre en el issue, con la valoración global.  
4. **Cierro el issue.** Eso indica que la revisión ha terminado.  
5. Concertamos la **entrevista de revisión** con el equipo.

Recibiréis notificación por correo en cada uno de esos pasos.

**Mientras el issue esté abierto, la revisión está en curso.** No hace falta que preguntéis si ya la he hecho: cuando esté hecha, os llegará el aviso.

>MUY IMPORTANTE: Es crucial que cada miembro del equipo responda la Coevaluación 360 al finalizar el proyecto. Mientras no esté respondida por todos los integrantes del equipo, su trabajo no será revisado por el profesorado.
---

## **10\. Consultar los comentarios de la revisión**

Cuando termine de revisar, recibiréis notificaciones por correo. Pero el correo solo avisa: los comentarios hay que leerlos en GitHub, y están en **dos sitios distintos**. Si solo miráis uno, os perderéis la mitad.

- **El issue de entrega** — la valoración global.  
- **Los comentarios sobre el código** — anotaciones puntuales sobre líneas concretas.

**Esto lo lee todo el equipo, no solo el coordinador.**

### **10.1. La valoración global (el issue)**

1. Entra en el repositorio en **github.com**.  
2. Pestaña **Issues**.  
3. Si no ves nada, es porque el issue está cerrado: pulsa el filtro **Closed** en la parte superior de la lista.  
4. Abre el de vuestra entrega y **lee el hilo entero de arriba abajo**.

Ahí encontraréis la valoración de conjunto y, normalmente, la lista de puntos comentados en el código con la indicación de dónde está cada uno.

### **10.2. Los comentarios sobre líneas de código**

Estos van pegados a una línea concreta de un archivo, en un momento concreto del historial. La forma fiable de llegar a todos:

1. Entra en el repositorio.  
2. Sobre la lista de archivos, pulsa donde pone **Commits** (con un icono de reloj y un número).  
3. Verás la lista de todas las subidas. Los commits que tienen comentarios muestran a la derecha **un icono de bocadillo con un número**.  
4. Pulsa sobre ese commit. Se abre la vista del cambio y **los comentarios aparecen insertados justo bajo la línea a la que se refieren**.

Recorred así todos los commits con bocadillo. Suele ser uno o dos, los últimos de cada archivo.

**Atajo:** los enlaces del correo de notificación llevan directamente a cada comentario. Si conserváis esos correos, es la vía más rápida.

### **10.3. Los comentarios no se responden en GitHub: se defienden en la entrevista**

Hay una diferencia importante respecto a lo que quizá esperáis. **No quiero que me respondáis por escrito a las correcciones.** Ni en el issue, ni bajo los comentarios de código.

La revisión se cierra en una **entrevista personal con el equipo**, en la que seréis vosotros quienes me expliquéis:

- Qué he señalado y por qué creéis que lo he señalado.  
- Qué error concreto había, en vuestras palabras.  
- Cómo lo resolveríais.

No basta con haber leído los comentarios: hay que entenderlos y traer propuestas. Un *"sí, teníamos un error en el filtrado"* no vale; sí vale *"el error era que excluíamos los NA antes de agrupar, y habría que hacerlo después, así: \[...\]"*.

> **Y aquí lo esencial: puedo preguntar a cualquiera por cualquier parte del proyecto.** No solo por el archivo que firma. El informe lo firmáis todos, el examen final es sobre el proyecto completo, y la entrevista funciona igual. Si habéis mantenido las reuniones semanales de la norma 7.2, esto no os supondrá ningún problema; si cada uno ha ido por libre, se verá de inmediato.

**Qué sí podéis escribir en GitHub**

Solo una cosa: si un comentario mío **no se entiende** —no que no estéis de acuerdo, sino que literalmente no sabéis a qué me refiero—, preguntadlo bajo ese comentario. Os aclararé el sentido para que podáis preparar la entrevista, pero el fondo no se discute ahí.

**Y no corrijáis los archivos.** El repositorio queda tal como se entregó. La corrección se demuestra explicándola, no subiendo una versión nueva.

### **10.4. Cómo preparar la entrevista en equipo**

Esta es la parte que más rendimiento os dará de cara a los dos proyectos siguientes y al examen final.

**Reuníos antes de la entrevista, con la revisión delante.** Recorred los comentarios uno a uno y acordad, para cada uno:

- **Qué señala exactamente.** Un comentario breve puede apuntar a un problema de fondo; aseguraos de captar el alcance.  
- **Por qué ocurrió.** Distinguid entre un descuido, un concepto estadístico mal aplicado y un fallo de coordinación. Se corrigen de formas distintas.  
- **Qué haríais ahora.** Una propuesta concreta: qué línea cambiaríais, qué método usaríais, qué habría que rehacer.

**Acordad las respuestas en equipo, no individualmente.** La propuesta de solución a cada comentario debe ser del equipo, discutida entre todos, aunque el error estuviera en el archivo de una sola persona. Ese es el sentido de la reunión.

Anotadlo en un documento compartido del equipo (no hace falta subirlo al repositorio).

**Que cada uno pueda explicar cualquier parte.** Podéis repartir quién arranca con cada tema, pero no os organicéis como si cada uno fuera a responder solo de lo suyo, porque no será así. Si en alguna cuestión el equipo se apoya siempre en la misma persona, esa es la que hay que trabajar antes de venir.

**Ensayad en voz alta las explicaciones difíciles.** Explicárselo a un compañero es la forma más rápida de descubrir que no lo tenías tan claro. Mejor descubrirlo en la reunión que en la entrevista.

**Atención a lo que comente sobre el funcionamiento del equipo** —reparto desequilibrado, bitácora descuidada, todo subido el último día, o precisamente que no todos dominan el conjunto—. Eso también forma parte de la entrevista, y es donde más margen de mejora tenéis, porque no depende de saber estadística.

**Después de la entrevista**, anotad las conclusiones en la primera entrada de la bitácora del Proyecto 2\. No es un trámite: es lo que evita que las mismas correcciones vuelvan a aparecer.

### **10.5. Y para el proyecto siguiente**

El repositorio del Proyecto 1 queda como está, con sus comentarios, para que podáis volver a él cuando queráis.

Para el Proyecto 2 recibiréis **una invitación a un repositorio nuevo**, con un **coordinador distinto**. Lo que cambia respecto a esta primera vez:

- No hay que crear cuenta ni instalar nada (pasos 1 y 3): ya está hecho.  
- Sí hay que aceptar la invitación, clonar en una carpeta nueva y crear el `.Rproj` (pasos 2, 4 y 5).  
- El resto del flujo es idéntico.

Os llevará quince minutos en lugar de una tarde.

---

## **Resumen de una página**

| Momento | Qué haces |
| :---- | :---- |
| **Al principio del curso** | Cuenta en GitHub → me mandas tu usuario → instalas GitHub Desktop |
| **Al empezar cada proyecto** | Aceptas la invitación → clonas → el coordinador crea el `.Rproj` |
| **Cada día de trabajo** | Pull → abrir `.Rproj` → trabajar → guardar → commit → **push** |
| **Cada semana** | Reunión del equipo \+ el coordinador actualiza la bitácora |
| **Una semana antes** | Se cierra el análisis y empieza la redacción del informe |
| **Al entregar** | Checklist (9.1) → reunión final → el coordinador abre el issue |
| **Al recibir la revisión** | Leer issue **y** comentarios de código → preparar la entrevista en equipo |

**Las tres cosas que más fallan, por si no recuerdas nada más:**

1. Olvidar el **push** al terminar (tu trabajo no existe para nadie).  
2. Dejar el **informe** para la última semana.  
3. Llegar a la entrevista **sabiendo solo tu parte**.

