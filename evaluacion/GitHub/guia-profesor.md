# Guía interna del profesor

**Montaje de repositorios, gestión de estudiantes y revisión comentada**

---

## 0. Antes de nada: ¿cuenta personal u organización?

Te recomiendo **crear una organización** en lugar de trabajar desde tu cuenta personal. Es gratuita y te ahorra bastante trabajo:

- Los repositorios quedan agrupados bajo un mismo nombre (`GLM2026/glm-proy1-equipo3`), no mezclados con los tuyos.
- Puedes crear **Teams** (equipos): defines una vez el Equipo 3 con sus cuatro miembros, y luego das acceso al equipo entero a cada repositorio de un clic, en lugar de invitar a cuatro personas tres veces cada una.
- Cuando acabe el curso, archivas la organización entera.

**Cómo crearla:** en github.com, tu foto de perfil → **Your organizations** → **New organization**. Elige plan **Free**: es el que recomienda el propio Quickstart docente de GitHub y ya incluye repositorios privados ilimitados y Teams (punto 1.2). Ponle un nombre reconocible; en este curso es **`GLM2026`**.

Con 3 proyectos × N equipos, la diferencia entre invitar individualmente y usar Teams es sustancial. Si tienes 6 equipos, hablamos de 54 invitaciones frente a 18 asignaciones de equipo — y las 6 creaciones de team se hacen una sola vez, siempre que mantengas **una única organización para todo el curso** (punto 1.4).

---

## 1. GitHub Education y el montaje de la organización

**Cuenta docente ya verificada.** Lo que sigue parte de ahí.

### 1.1. Por qué no encuentras dónde «solicitar Team para la organización»

Porque, tal como está documentado hoy, **ese trámite separado no existe**, y en tu caso probablemente **no lo necesitas**. Conviene desmontarlo con calma, porque es fácil perder una tarde buscando un botón que no está.

Lo que dice la documentación de GitHub, literalmente:

- El beneficio docente se describe como «Apply for free **GitHub Team**, which allows unlimited users and private repositories».
- Pero **el único formulario documentado es el personal**, el que ya has completado: *Settings → [Billing settings](https://github.com/settings/billing/summary) → «GitHub Education» → **Start an application***. También accesible en [github.com/settings/education/benefits](https://github.com/settings/education/benefits).
- Y el **Quickstart para docentes** de GitHub, cuando te dice cómo montar el curso, indica textualmente crear una organización **gratuita** («Follow the prompts to create a **free** organization»). No menciona ningún paso posterior de solicitud para esa organización.

### 1.2. Y sobre todo: GitHub Free para organizaciones ya te da todo lo que usas

Esta es la parte que resuelve el problema. Comparando las dos fichas de plan en la documentación:

| Lo que necesita el curso | ¿Está en **GitHub Free para organizaciones**? |
|---|---|
| Repositorios privados ilimitados | Sí |
| Colaboradores ilimitados | Sí |
| **Teams** (grupos con permisos) | Sí — «Team access controls for managing groups» |
| Issues y comentarios sobre líneas de código | Sí |

Y lo que **añade Team** sobre Free para organizaciones es: soporte por email, 3.000 minutos de Actions en vez de 2.000, 2 GB de Packages, revisores obligatorios de pull request, ramas protegidas, *code owners*, wikis y Pages en repositorios privados.

**Nada de eso interviene en nuestro flujo.** Los equipos hacen *push* directo a `main`, no usamos pull requests ni ramas protegidas, no ejecutamos Actions, y Pages ya quedó descartado en 5.1 porque publicaría el sitio en internet.

**Conclusión operativa:** monta la organización en plan **Free** y sigue adelante. No estás renunciando a nada que uses.

> [VERIFICAR] Si aun así quieres intentar el ascenso a Team, el camino a probar es Organización → **Settings** → **Billing and plans** → **Upgrade**, a ver si GitHub reconoce ahí la cuenta docente verificada. No he podido comprobarlo, y la documentación no lo describe. Si no aparece, pregunta en la [Education Community](https://github.com/orgs/community/discussions/categories/github-education), que es donde GitHub atiende estos casos.

### 1.3. ¿Se pueden agrupar los repositorios en carpetas por proyecto?

**No: GitHub no tiene carpetas ni subgrupos de repositorios dentro de una organización.** La lista es plana. (Es una diferencia real con GitLab, que sí tiene subgrupos anidados; si vienes de ahí, la ausencia despista.)

Pero el objetivo —no mezclar proyectos— se consigue igual, y mejor, con tres mecanismos que sí existen:

**a) El prefijo en el nombre.** Ya lo estás haciendo con `glm-proy1-equipo3`. La lista de repositorios se ordena alfabéticamente, así que los tres bloques salen agrupados solos, y el buscador de la organización filtra escribiendo `proy1`.

**b) Los *topics*.** A cada repositorio le pones la etiqueta `proyecto-1` (pestaña **Code** del repositorio → engranaje junto a «About» → *Topics*). Después, en la pestaña **Repositories** de la organización tienes un filtro por topic, y la URL

```
https://github.com/orgs/GLM2026/repositories?q=topic%3Aproyecto-1
```

te deja una vista con **solo** los repositorios de ese proyecto. Guárdala en marcadores y funciona exactamente como la carpeta que buscas.

**c) Archivar al cerrar el bloque.** Esta es la que más te va a servir, porque los tres proyectos son **consecutivos en el tiempo**. Cuando termines de corregir el Proyecto 1: *Settings → Danger Zone → **Archive this repository***. El repositorio pasa a solo lectura —se conserva todo, comentarios incluidos— y **desaparece de la lista por defecto** de la organización. Al empezar el Proyecto 2, la vista de repositorios muestra únicamente lo vivo.

Combinando (a) y (c) tienes el efecto de las carpetas sin necesitar carpetas: la lista siempre enseña el proyecto en curso, y los anteriores están a un filtro de distancia.

### 1.4. Entonces, ¿una organización o tres?

Con lo anterior, **una sola organización para todo el curso** —`GLM2026`, que es la adoptada— es la opción clara:

- Los **Teams se definen una única vez** y se reutilizan en los tres proyectos. Con tres organizaciones los recreas tres veces.
- Los estudiantes aceptan **una** invitación de organización, no tres.
- El aislamiento entre proyectos lo dan el prefijo, los topics y el archivado, no la organización.

La alternativa sería una organización por proyecto (`GLM-PROYECTO1`, `GLM-PROYECTO2`…). Funciona, pero obliga a recrear los Teams en cada bloque y a que los estudiantes acepten tres invitaciones de organización. Toda la documentación del curso —guía del estudiante, bitácora y plantillas de informe— asume ya `GLM2026`.

### 1.5. Los Teams: definirlos una vez, usarlos tres

Este es el ahorro real de trabajar con organización, y **está disponible en el plan Free**. Ver el punto 3 para el detalle operativo, pero la idea es:

1. **Una vez por curso:** creas `equipo-1`, `equipo-2`… con sus miembros.
2. **Una vez por repositorio:** *Settings → Collaborators and teams → Add teams →* el equipo, con rol **Write**.

Con 6 equipos y 3 proyectos, eso son 6 creaciones de team y 18 asignaciones, frente a las 54 invitaciones individuales del camino manual.

### 1.6. Sobre GitHub Classroom

Classroom automatiza justamente lo que hace el punto 2 de esta guía: crea un repositorio por equipo a partir de una plantilla, asigna los miembros y te da un panel con el estado de todas las entregas.

**Mi recomendación sigue siendo montar el Proyecto 1 a mano.** Es media hora de trabajo y entiendes exactamente qué está pasando en cada paso, lo que te deja en condiciones de arreglar cualquier cosa que se tuerza. Si para el Proyecto 2 quieres migrar a Classroom, valora que añade su propia curva de aprendizaje y que a esas alturas ya tendrás el proceso manual rodado.

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

## 5. Ver el informe y la presentación

### 5.1. La restricción de partida

Conviene tenerla clara, porque condiciona todo lo demás:

- **GitHub no renderiza el HTML alojado.** La vista de archivo muestra el código fuente y la vía `raw` lo entrega como texto plano, deliberadamente, para que nadie sirva páginas ejecutables desde sus dominios. Ser propietaria del repositorio no lo cambia: no es un permiso que puedas levantar.
- **`htmlpreview.github.io` solo funciona con repositorios públicos.** Con los nuestros devuelve página en blanco o error. No es una opción.
- **GitHub Pages sí renderiza**, y funciona desde repositorios privados con GitHub Pro, Team o Enterprise Cloud. Pero **publicar el sitio *en privado*** —accesible solo a quien tenga lectura del repositorio— **requiere Enterprise Cloud**. Ni con Free ni con Team —que es lo máximo que da el beneficio docente— el sitio publicado sería privado: quedaría **público en internet**. Descartado para trabajos de estudiantes.
- **Los `.pdf` sí los abre y los pinta** dentro de GitHub, también en repositorios privados, para quien tenga acceso.

> [VERIFICAR] Si la UMH llegara a ser **GitHub Campus Program**, la organización podría disponer de Enterprise Cloud y entonces sí tendrías Pages con control de acceso: enlace privado y HTML íntegro, que sería la solución ideal. Merece una consulta al servicio de informática.

### 5.2. La estrategia adoptada: los dos formatos

Los equipos entregan **ambos**, y enlazan ambos en el issue (9.3 de su guía):

| Archivo | Para qué sirve | Cómo lo abres |
|---|---|---|
| `informe.pdf` | Lectura y corrección rápida | Un clic en GitHub. Se ve paginado en el navegador. |
| `informe.html` | El documento auténtico: TOC, código plegable, tablas paginadas | Descarga local (5.3) |
| `presentacion.pdf` | Siempre presente, como red de seguridad | Un clic en GitHub |
| Enlace a la nube | Solo si la presentación está en Slides, Canva… Conserva animaciones e interactividad | El enlace del issue |

Los tres enlaces alojados en GitHub tienen la misma forma, y el coordinador los copia de la barra del navegador para que no haya erratas:

```
https://github.com/GLM2026/glm-proy1-equipo3/blob/main/informe.pdf
https://github.com/GLM2026/glm-proy1-equipo3/blob/main/informe.html
https://github.com/GLM2026/glm-proy1-equipo3/blob/main/presentacion.pdf
```

El PDF cubre el 90 % de la corrección. El HTML lo abres cuando quieras ver el documento como lo construyeron: código plegado, tablas navegables, cualquier gráfico interactivo que el PDF aplana.

### 5.3. Abrir el HTML: a mano

Entra al `informe.html` en GitHub → botón **Download raw file** (icono de flecha, arriba a la derecha de la vista del archivo) → ábrelo desde tu ordenador con el navegador.

Con `embed-resources: true` en la plantilla, ese único archivo se abre perfectamente, sin carpeta de recursos que arrastrar. Son dos clics por informe: con 6 equipos y 3 proyectos, unas 18 descargas en todo el curso. Perfectamente asumible si prefieres no tocar la terminal.

### 5.4. Abrir el HTML: todos de una vez, con `gh`

Si vas a corregir una ronda entera, compensa traértelos todos a una carpeta local de un tirón. Necesitas la **CLI de GitHub** ([cli.github.com](https://cli.github.com)), instalada y autenticada una sola vez:

```bash
gh auth login          # una vez, elige GitHub.com y autenticación por navegador
```

Y después, por cada ronda de corrección:

```bash
ORG=GLM2026                       # la organización del curso
mkdir -p ~/correccion/$ORG && cd ~/correccion/$ORG

gh repo list "$ORG" --limit 100 --json name -q '.[].name' | while read -r repo; do
  gh api "repos/$ORG/$repo/contents/informe.html" \
     -H "Accept: application/vnd.github.raw" > "$repo.html" \
     && echo "OK    $repo" \
     || echo "FALTA $repo"
done
```

Te deja un `.html` por equipo, con el nombre del repositorio, listo para abrir con doble clic. Y como efecto secundario útil, la columna `FALTA` te dice **qué equipos no han subido el informe**, que es justo lo que quieres saber antes de empezar a corregir.

> [VERIFICAR] No he podido ejecutar este bloque contra un repositorio real. Pruébalo con un solo equipo antes de fiarte de él en una ronda completa.

Para llevarte también los PDF, cambia `informe.html` por `informe.pdf` y la extensión del fichero de salida. Y si prefieres tener los repositorios enteros —código incluido— en lugar de solo los informes, `gh repo clone "$ORG/$repo"` dentro del mismo bucle hace eso.

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

## 7. Checklist de montaje

### Una sola vez, al empezar el curso

- [ ] Crear la organización del curso (punto 0)
- [ ] Dejarla en plan **Free**: cubre todo lo que usa el curso (punto 1.2)
- [ ] Crear un **Team** por equipo, con sus miembros (punto 1.5)
- [ ] Decidir el criterio de agrupación: prefijo en el nombre y *topic* por proyecto (punto 1.3)
- [ ] Instalar y autenticar la **CLI de `gh`** si vas a usar la descarga masiva de informes (punto 5.4)

### Por proyecto

Para cada uno de los tres proyectos:

- [ ] Crear `plantilla-proyN` con README, `plantilla_informe.qmd` y `bitacora.md`
- [ ] Settings → marcar **Template repository**
- [ ] Verificar que `plantilla_informe.qmd` lleva `embed-resources: true`
- [ ] Verificar que `bitacora.md` lleva al pie el bloque de cierre con la plantilla del issue
- [ ] Generar `proyN-equipoX` con **Use this template**, uno por equipo
- [ ] Ponerle a cada uno el *topic* `proyecto-N` (punto 1.3)
- [ ] Dar acceso **Write** al team (o a los colaboradores) de cada equipo
- [ ] Avisar por el campus virtual de que la invitación caduca en 7 días
- [ ] Confirmar el coordinador de turno de cada equipo (rota en cada proyecto)
- [ ] Comprobar en la semana 1 que todos han aceptado y que el `.Rproj` está subido

Ese último punto es el mejor indicador temprano: si en la semana 1 el `.Rproj` no aparece, ese equipo aún no ha arrancado.

### Al cerrar cada proyecto

- [ ] Descargar los informes de todos los equipos (punto 5.4) antes de tocar nada
- [ ] Cerrar los issues de entrega tras la entrevista de revisión
- [ ] **Archivar** los repositorios del proyecto, para que desaparezcan de la lista viva (punto 1.3)
