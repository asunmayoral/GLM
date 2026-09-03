# Cómo trabajar el proyecto con GitHub

Modelos Lineales Generalizados · Grado en Ciencia de Datos e IA

Guía para el trabajo en equipo. El repositorio de vuestro equipo ya está creado y
tenéis acceso: no hay que crear nada, solo aprender a usarlo.

---

## 0 · Preparación (una sola vez)

1. Crea una cuenta en [github.com](https://github.com) y **manda tu usuario la primera
   semana**. Sin él no se te puede dar acceso al repositorio.

2. Instala **Git** ([git-scm.com](https://git-scm.com)) y dile quién eres. En la
   terminal, o en la pestaña *Terminal* de RStudio:

   ```bash
   git config --global user.name "Ana Ruiz"
   git config --global user.email "ana.ruiz@alu.umh.es"
   ```

3. Abre el repositorio en RStudio: **File → New Project → Version Control → Git**, y
   pega la URL del repositorio de tu equipo.

Ya está. A partir de ahí trabajas en tu carpeta como siempre, y la pestaña **Git** de
RStudio te muestra qué has cambiado.

---

## 1 · Cómo repartiros el trabajo sin pisaros

Este es el apartado importante. Tres personas escribiendo el mismo `informe.qmd` es la
receta segura para pasar más tiempo arreglando conflictos que analizando datos.

### Trocead el informe

**No trabajéis nunca sobre un único fichero grande.** Partid el informe en secciones y
que cada una sea un fichero:

```
informe.qmd          ← solo el YAML y las llamadas
_01-problema.qmd
_02-datos.qmd
_03-modelo.qmd
_04-resultados.qmd
_05-discusion.qmd
R/
datos/
```

El `informe.qmd` queda así de corto:

````markdown
---
title: "Título del informe"
author: "Equipo Nelder"
format:
  html:
    embed-resources: true
---

{{< include _01-problema.qmd >}}

{{< include _02-datos.qmd >}}

{{< include _03-modelo.qmd >}}
````

Quarto los pega en el orden en que aparecen y renderiza como si fuera un solo
documento. **El guion bajo del nombre importa**: le dice a Quarto que no los renderice
también por separado.

Con esto, dos personas trabajando a la vez tocan ficheros distintos y Git no tiene nada
que resolver. Es la medida que más conflictos evita, con diferencia.

> Dos detalles: los ficheros incluidos deben usar todos el mismo motor —todos R, en
> vuestro caso—, y las rutas relativas de dentro (imágenes, datos) se resuelven **desde
> la carpeta del `informe.qmd`**, no desde la del fichero incluido.

### Escribid una frase por línea

Truco poco conocido y muy eficaz. Git compara **líneas**. Si un párrafo entero es una
sola línea, cualquier cambio de uno choca con cualquier cambio de otro. Si cada frase va
en su línea, Git fusiona sin problema los cambios de dos personas en el mismo párrafo.

```markdown
El modelo logístico estima la probabilidad de reingreso.
La variable respuesta es binaria, por lo que la familia binomial es la natural.
Usamos el enlace logit para poder interpretar en odds ratios.
```

Al renderizar se ve exactamente igual que si fuera un párrafo corrido. Solo cambia cómo
lo ve Git.

### Un fichero, una persona, en cada momento

Antes de ponerte con `_03-modelo.qmd`, dilo en el chat del equipo. No es burocracia: es
lo que hace que la división por ficheros funcione de verdad.

### Lo que NO debéis hacer

> **No pongáis la carpeta del repositorio dentro de Google Drive, Dropbox o OneDrive.**

Esos servicios sincronizan ficheros uno a uno y sin entender qué es Git. Acaban
corrompiendo la carpeta `.git` y el repositorio deja de funcionar, a veces sin aviso
hasta que ya has perdido trabajo. **Git ya es vuestro sistema de sincronización**; no
necesita otro encima, y los dos juntos se estorban.

---

## 2 · El ritual, cada vez que os sentáis a trabajar

```bash
git pull                       # 1. Traer lo que hayan hecho los demás
                               # 2. Trabajar
git add _03-modelo.qmd
git commit -m "Añade el diagnóstico de residuos del modelo Poisson"
git push                       # 3. Subirlo
```

En RStudio, pestaña **Git**: botón **Pull** (flecha azul abajo) → trabajar → marcar
casillas *Staged* → **Commit** → **Push** (flecha verde arriba).

Tres reglas que valen por todo lo demás:

**`pull` antes de empezar, siempre.** El 90 % de los conflictos vienen de trabajar tres
días sobre una versión vieja.

**`push` al terminar, siempre.** Lo que está solo en tu portátil no existe para el
equipo.

**Commits pequeños y frecuentes.** Uno por cosa terminada, no uno al final del día con
todo dentro. Cuanto más pequeño el cambio, más fácil de fusionar y de deshacer si sale
mal.

Y el mensaje dice **qué cambia y por qué**:

| No | Sí |
|---|---|
| `Cambios` | `Añade el offset de exposición al modelo Poisson` |
| `informe` | `Reescribe la interpretación del IRR en escala de tasas` |
| `arreglos varios` | `Corrige la ruta absoluta que rompía el render` |

---

## 3 · Cuando aparece un conflicto

Pasará alguna vez. No es una avería: es Git diciendo «dos personas cambiaron lo mismo y
no decido yo».

Verás esto dentro del fichero:

```
<<<<<<< HEAD
La odds ratio es de 1,84.
=======
La razón de odds estimada es 1,84 (IC 95 %: 1,21–2,79).
>>>>>>> origin/bloque-1
```

Arriba, lo tuyo. Abajo, lo de la otra persona. Para resolverlo:

1. Decide qué texto queda —el tuyo, el otro, o una mezcla—.
2. **Borra las tres líneas de marcas** (`<<<<<<<`, `=======`, `>>>>>>>`).
3. `git add` del fichero y `git commit`.

Si te agobia, para y pregunta antes de tocar nada. Un conflicto mal resuelto borra
trabajo de un compañero sin que se note.

---

## 4 · ¿Local o en la nube?

**Trabajad en local, con RStudio.** Es lo que vais a usar profesionalmente, funciona sin
conexión y no depende de horas de servidor.

Dos excepciones razonables:

**Para redactar prosa a varias manos en el mismo rato**, usad un documento colaborativo
de verdad —Google Docs, HackMD— y cuando el texto esté cerrado, una sola persona lo pasa
al `.qmd`. Intentar escribir en tiempo real a través de Git es pelear contra la
herramienta. Pero que sea solo para prosa: **el código y los resultados viven en el
repositorio**, o perdéis la reproducibilidad, que es criterio de la rúbrica.

**Si alguien no puede instalar R en su equipo**, Posit Cloud abre RStudio en el navegador
y conecta con GitHub igual. El plan gratuito va justo de memoria y de horas, así que como
plan B.

---

## 5 · Cómo se entrega el proyecto

Cada bloque se entrega abriendo un **pull request** en el repositorio de vuestro equipo.
No se entrega por correo ni en el aula virtual.

### 1. Trabajad en una rama llamada `bloque-N`, nunca directamente en `main`

```bash
git checkout main
git pull
git checkout -b bloque-1        # crearla, solo la primera vez
git push -u origin bloque-1     # publicarla, solo la primera vez
```

Los demás se incorporan con `git checkout bloque-1` después de un `git pull`. En
RStudio, el botón de rama de la pestaña Git hace lo mismo.

### 2. Haced commits pequeños y con mensajes que digan qué cambia y por qué

«Cambios» no es un mensaje. Ver la tabla del apartado 2.

### 3. Cuando esté listo, abrid el PR

Con el título `Bloque N · Equipo X · Entrega del informe`.

Antes de abrirlo, comprobad tres cosas:

- Que el informe **renderiza desde cero** en una carpeta limpia. Si solo funciona en el
  portátil de uno, no es reproducible.
- Que **no hay rutas absolutas** (`C:/Users/...`) ni aleatoriedad sin `set.seed()`.
- Que las **cifras del texto coinciden** con las que produce el código. Es el error más
  caro: invalida la lectura sin que se note.

Con todo verde, `git push`, y GitHub os ofrecerá el botón **Compare & pull request**. La
descripción responde a cuatro preguntas:

```
- Pregunta que responde el informe:
- Banco de datos y unidad de análisis:
- Modelo ajustado, y por qué esa familia y ese enlace:
- Qué nos ha quedado dudoso:
```

La última no es un trámite. Decir por dónde flojeáis se valora; disimularlo, no.

### 4. Recibiréis la corrección como comentarios anclados a líneas concretas

Respondedlos: se puede discrepar y argumentar, eso también se valora. Un «tienes razón,
corregido» y un «no lo vemos así, porque…» valen los dos; el silencio no.

### 5. Corregid en la misma rama y marcad *Resolve conversation*

No abráis otro PR: el que hay se actualiza solo al hacer `push`. Marcad **Resolve
conversation** en cada hilo atendido. Los hilos que queden abiertos se ven, y cuentan.

### 6. En el examen oral individual se os preguntará por alguno de esos hilos

Tenedlo presente al responderlos: lo que escribáis ahí es lo que tendréis que sostener en
voz alta.

---

El PR queda archivado con toda la conversación: es el registro de vuestro trabajo y de
cómo respondisteis a la corrección.

---

## 6 · Los cinco errores que os van a costar tiempo

| | Qué pasa | Cómo se evita |
|---|---|---|
| 1 | Trabajar sin hacer `pull` primero | Ritual: `pull` siempre al empezar |
| 2 | Un solo `.qmd` de mil líneas | Trocearlo con `include` |
| 3 | El repositorio dentro de Drive o Dropbox | Sacarlo de ahí hoy mismo |
| 4 | Subir los datos pesados | `.gitignore`; enlazad la fuente o el script que los descarga |
| 5 | Un único commit gigante el día de la entrega | Commits pequeños, desde el primer día |

Y uno que no es error sino sensación: **al principio Git parece que estorba**. Los tres
primeros días cuesta. A partir de ahí es lo que os deja saber quién cambió qué y volver
atrás cuando algo se rompe — que en un proyecto a tres manos pasa siempre.
