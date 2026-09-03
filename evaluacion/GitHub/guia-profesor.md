# Guía interna del profesor

**Montaje de repositorios, gestión de estudiantes y revisión comentada**

---

## 0. Antes de nada: ¿cuenta personal u organización?

Te recomiendo **crear una organización** en lugar de trabajar desde tu cuenta personal. Es gratuita y te ahorra bastante trabajo:

- Los repositorios quedan agrupados bajo un mismo nombre (`estadistica-2026/proy1-equipo3`), no mezclados con los tuyos.
- Puedes crear **Teams** (equipos): defines una vez el Equipo 3 con sus cuatro miembros, y luego das acceso al equipo entero a cada repositorio de un clic, en lugar de invitar a cuatro personas tres veces cada una.
- Cuando acabe el curso, archivas la organización entera.

**Cómo crearla:** en github.com, tu foto de perfil → **Your organizations** → **New organization** → plan **Free**. Ponle un nombre reconocible (`estadistica-uni-2026`).

Con 3 proyectos × N equipos, la diferencia entre invitar individualmente y usar Teams es sustancial. Si tienes 6 equipos, hablamos de 72 invitaciones frente a 18 asignaciones de equipo.

---

## 1. GitHub Education: lo que aporta y lo que no

Merece la pena solicitarlo, pero **no lo esperes para empezar**: la verificación puede tardar días.

**Qué se solicita:** en `education.github.com`, la opción **Teacher** (GitHub Global Campus). Te pedirán una prueba de tu vinculación docente (correo institucional, credencial, o una foto de tu documento de profesor).

**Lo que aporta que te interese:**

- **GitHub Classroom**, que automatiza justamente lo que vas a hacer a mano: crea un repositorio por equipo a partir de una plantilla, asigna los miembros y te da un panel con el estado de todas las entregas.
- Organizaciones con funciones de pago gratis para el ámbito educativo.

**Lo que NO necesitas de ahí:** los repositorios privados con colaboradores ilimitados ya son gratuitos para todo el mundo. No dependas de Education para eso.

**Mi recomendación práctica:** monta el Proyecto 1 a mano siguiendo esta guía. Es media hora de trabajo y entiendes exactamente qué está pasando. Si para el Proyecto 2 ya tienes Classroom aprobado, valora si te compensa migrar — sabiendo que Classroom añade su propia curva de aprendizaje y que a esas alturas ya tendrás el proceso manual rodado.

---

## 2. Crear el repositorio plantilla

Necesitas **tres plantillas**, una por proyecto, porque cada una lleva su propio `plantilla_informe.qmd` y su README con el enunciado.

### 2.1. Crea el repositorio

1. En tu organización (o cuenta), botón **New repository**.
2. **Repository name**: `plantilla-proy1`.
3. **Visibility**: elige **Private**. Lo comento en el punto 5, pero es la opción por defecto razonable en docencia.
4. Marca **Add a README file** para que no nazca vacío.
5. **Create repository**.

### 2.2. Sube los archivos

La forma más rápida, desde la web: botón **Add file** → **Upload files** → arrastra los archivos → **Commit changes**.

Contenido de cada plantilla:

| Archivo | Qué contiene |
|---|---|
| `README.md` | La guía del estudiante + el enunciado del proyecto |
| `plantilla_informe.qmd` | El molde del informe de ese proyecto |
| `bitacora.md` | Cabecera vacía con el formato de la tabla semanal |

En `plantilla_informe.qmd`, añade `embed-resources: true` en la cabecera YAML. Así el `Render` genera **un único archivo HTML** en lugar de un `.html` más una carpeta `_files` con decenas de recursos. Reduce muchísimo el ruido en el repositorio y te facilita la descarga.

### 2.3. Márcalo como plantilla

Este es el paso que lo convierte en reutilizable:

**Settings** (pestaña del repositorio) → en la primera sección, marca la casilla **Template repository**.

A partir de ahí, el repositorio muestra un botón verde **Use this template**.

### 2.4. Genera un repositorio por equipo

Para cada equipo:

1. Entra en `plantilla-proy1` → **Use this template** → **Create a new repository**.
2. **Owner**: tu organización.
3. **Repository name**: `proy1-equipo3` (mantén una nomenclatura estricta; la agradecerás con 18 repositorios).
4. **Private**.
5. **Create repository**.

Tarda unos segundos por equipo. El contenido de la plantilla se copia íntegro, con historial limpio.

---

## 3. Dar acceso a los estudiantes

### Si usas organización (recomendado)

**Primero, crea el equipo (una sola vez por curso):**

1. En la organización → pestaña **Teams** → **New team** → nombre `equipo-3`.
2. Dentro del equipo → **Add a member** → busca por nombre de usuario de GitHub.

**Después, en cada repositorio:**

**Settings** → **Collaborators and teams** → **Add teams** → selecciona `equipo-3` → rol **Write**.

Hecho. Los cuatro tienen acceso de una vez, y en los proyectos 2 y 3 repites solo este último paso.

### Si usas cuenta personal

**Settings** → **Collaborators** → **Add people** → escribe el nombre de usuario → rol **Write**.

Uno por uno, y para cada repositorio. GitHub autocompleta los usuarios que ya has invitado antes, lo que alivia algo el trabajo.

### Sobre los permisos

**Write** es el rol correcto: permite subir código, abrir issues y comentar, pero no borrar el repositorio ni cambiar su configuración.

No les des **Admin**. No lo necesitan y sí les permite hacer cosas irreversibles.

### Detalle importante

Las invitaciones **caducan a los 7 días**. Manda un aviso por el campus virtual el mismo día que invites, porque el correo de GitHub se pierde con facilidad en la bandeja de entrada.

---

## 4. Seguir el trabajo durante el curso

No hace falta que entres a diario. Tres formas de mirar, de menos a más detalle:

**El pulso general.** Entra al repositorio y mira la lista de archivos: a la derecha de cada uno aparece el mensaje del último commit y cuándo fue. Si todo dice "hace 3 semanas", el equipo está parado.

**Quién ha hecho qué.** Pestaña **Insights** → **Contributors**. Te da un gráfico de aportaciones por persona a lo largo del tiempo. Es la forma más rápida de detectar un reparto desequilibrado, y una conversación incómoda que conviene tener en la semana 4 y no en la 12.

**La bitácora.** `bitacora.md` es tu fuente principal. Ábrela desde la web (se ve formateada) y revisa la sección de dudas. Un vistazo semanal a las bitácoras de todos los equipos te lleva diez minutos.

**Activar avisos:** en cada repositorio, botón **Watch** (arriba a la derecha) → **All Activity** si quieres enterarte de todo, o **Participating and @mentions** si prefieres que solo te lleguen los issues y las menciones. Con muchos repositorios, la segunda opción es más llevadera.

---

## 5. Ver el informe renderizado

Aquí hay una pega de GitHub que conviene conocer: **un archivo `.html` alojado en el repositorio no se muestra como página web**. Al abrirlo verás el código fuente.

**Si los repositorios son privados** (lo recomendable): tendrás que descargar el archivo. Entra al `informe.html` → botón **Download raw file** (icono de flecha, arriba a la derecha de la vista del archivo) → ábrelo desde tu ordenador con el navegador.

Con `embed-resources: true` en la plantilla, ese único archivo se abre perfectamente. Sin esa opción, tendrías que descargar también la carpeta de recursos y sería un engorro.

**Si fueran públicos**: bastaría con anteponer `https://htmlpreview.github.io/?` a la URL del archivo para verlo renderizado en el navegador, sin descargar nada. Es cómodo, pero no lo consideraría razón suficiente para hacer públicos los trabajos de tus estudiantes.

---

## 6. Comentar el trabajo: la parte que más te importa

Aquí está el mecanismo central. GitHub permite dejar comentarios **anclados a una línea concreta de un archivo**, sin necesidad de pull requests. Es exactamente lo que necesitas, pero la ruta no es evidente.

### 6.1. Cómo dejar un comentario sobre una línea de código

1. Entra en el repositorio del equipo.
2. Sobre la lista de archivos, pulsa **Commits** (icono de reloj con un número).
3. Verás la lista de subidas. **Pulsa sobre el commit** donde está el trabajo que quieres comentar — normalmente el último que tocó ese archivo.
4. Se abre la vista del cambio (*diff*): a la izquierda lo anterior, a la derecha lo nuevo.
5. **Pasa el ratón sobre la línea** que quieres comentar. A la izquierda del número de línea aparece un **botón azul con un `+`**.
6. Púlsalo. Se abre un recuadro de texto.
7. Escribe el comentario y pulsa **Comment on this commit**.

El comentario queda pegado a esa línea. Los estudiantes lo verán exactamente ahí, con el código delante.

**Para comentar un bloque de varias líneas**: pulsa el `+` de la primera línea y **arrastra** hasta la última antes de soltar. El comentario abarcará todo el rango.

### 6.2. Tres cosas que conviene saber

**Los comentarios de commit no aparecen en la pestaña Issues.** Viven solo en la vista del commit. Por eso en la guía del estudiante les explico cómo recorrer los commits buscando el icono de bocadillo, y por eso te recomiendo el punto 6.4.

**Comentas el `.qmd`, no el `.html`.** El HTML renderizado no se puede anotar línea a línea de forma útil. Léelo para valorar el resultado, pero deja los comentarios sobre el código fuente del informe y de los análisis.

**Los comentarios notifican automáticamente** a quienes estén siguiendo el repositorio (que serán todos los colaboradores por defecto). No necesitas avisar aparte.

### 6.3. Cómo enlazar a una línea concreta desde otro sitio

Muy útil para el comentario de cierre. Si quieres referirte a una línea exacta:

1. Abre el archivo en la web.
2. **Pulsa sobre el número de línea** (o arrastra para seleccionar varias). La línea se resalta y la URL cambia, terminando en `#L42`.
3. Copia esa URL.

Al pegarla en un issue, GitHub muestra automáticamente un recuadro con el fragmento de código. Es la forma más clara de decir "esto de aquí".

**Consejo:** en el menú `...` que aparece junto a la línea seleccionada, elige **Copy permalink**. Ese enlace apunta a la versión exacta del archivo en ese momento y no se rompe si después cambia el contenido.

### 6.4. El comentario de cierre en el issue

Los comentarios de línea son precisos pero están dispersos. La valoración global va en el issue de entrega, y ahí es donde te recomiendo dejar el **índice** de lo comentado.

Estructura sugerida:

```markdown
## Revisión — Proyecto 1, Equipo 3

**Valoración general**
[Dos o tres párrafos: qué funciona, qué no, impresión de conjunto]

**Comentarios en el código**
He dejado anotaciones en los siguientes puntos:
- Cuestión 2, tratamiento de los NA → [enlace al comentario]
- Cuestión 3, elección del modelo → [enlace al comentario]
- Informe, apartado 4, interpretación del p-valor → [enlace al comentario]

**Sobre el funcionamiento del equipo**
[Reparto, bitácora, ritmo de trabajo]

**Para la entrevista**
Venid preparados especialmente sobre [X] e [Y].

@martasanchez @javierlopez @luciaperez
```

Para obtener el enlace de un comentario ya publicado: pulsa sobre la **marca de tiempo** del comentario (o el menú `...` → **Copy link**).

Las menciones con `@` al final aseguran que a todos les llegue la notificación, no solo a quien abrió el issue.

Cuando termines, **cierra el issue** con el botón **Close issue**. Para los estudiantes, esa es la señal de que la revisión está completa.

### 6.5. Flujo de revisión recomendado

Un orden que funciona bien y evita rehacer trabajo:

1. **Lee la bitácora primero.** Te dice quién hizo qué y con qué dificultades. Cambia por completo cómo interpretas lo que vas a leer después.
2. **Descarga y lee el `informe.html`.** Valóralo como producto final, sin entrar todavía en el código.
3. **Repasa los `.qmd` de análisis** y ve dejando comentarios de línea sobre la marcha.
4. **Comprueba el historial de commits.** Contrasta si el reparto declarado en la bitácora se corresponde con quién subió qué y cuándo. Aquí se ven los equipos donde uno solo trabajó, y los que hicieron todo en las últimas 48 horas.
5. **Escribe el comentario de cierre** con los enlaces, y cierra el issue.
6. **Convoca la entrevista.**

### 6.6. Sobre el tono de los comentarios

Un apunte, porque estos comentarios los leerá todo el equipo y quedan en el repositorio de forma permanente.

Como el modelo de evaluación es la entrevista, te conviene que los comentarios **abran preguntas más que las cierren**. En lugar de "esto está mal, debería ser una prueba no paramétrica", algo como "¿por qué habéis optado por un test paramétrico aquí? Revisad los supuestos". Les obliga a llegar a la entrevista con la respuesta pensada, en vez de con tu corrección memorizada.

Y ten presente que un comentario negativo dirigido a una persona lo leen sus tres compañeros. Para lo que sea personal —desequilibrio en el reparto, alguien que apenas ha aparecido—, mejor la entrevista o un correo privado.

---

## 7. Checklist de montaje, por proyecto

Para cada uno de los tres proyectos:

- [ ] Crear `plantilla-proyN` con README, `plantilla_informe.qmd` y `bitacora.md`
- [ ] Settings → marcar **Template repository**
- [ ] Verificar que `plantilla_informe.qmd` lleva `embed-resources: true`
- [ ] Generar `proyN-equipoX` con **Use this template**, uno por equipo
- [ ] Dar acceso **Write** al team (o a los colaboradores) de cada equipo
- [ ] Avisar por el campus virtual de que la invitación caduca en 7 días
- [ ] Confirmar el coordinador de turno de cada equipo (rota en cada proyecto)
- [ ] Comprobar en la semana 1 que todos han aceptado y que el `.Rproj` está subido

Ese último punto es el mejor indicador temprano: si en la semana 1 el `.Rproj` no aparece, ese equipo aún no ha arrancado.
