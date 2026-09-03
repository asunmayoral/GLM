# Coevaluación 360 · cómo se hace y cómo se califica

Modelos Lineales Generalizados · Grado en Ciencia de Datos e IA

La parte 1 se dirige a los estudiantes y puede repartirse tal cual. La parte 2 es el
procedimiento del profesorado; conviene recortarla antes de distribuir el documento.

---

# Parte 1 · Para los estudiantes

## 1. Para qué sirve

El proyecto de cada bloque se califica una vez, para todo el equipo. Eso es correcto
cuando los tres han trabajado, e injusto cuando no.

La coevaluación existe para corregir esa injusticia: **ajusta la nota del proyecto a
la implicación real de cada persona**. Se cumplimenta tres veces, una por bloque.

Conviene entender qué **no** mide, porque de ahí salen los malentendidos:

> **No valora la calidad del trabajo.** De eso ya se ocupan las rúbricas del informe y
> de la exposición. La coevaluación responde a otra pregunta: **¿hizo cada uno la parte
> que le correspondía?**

Por eso la escala habla de plazos, de reparto y de compromisos cumplidos, y no de si
el modelo estaba bien elegido. Si midiera calidad, la calidad contaría dos veces.

## 2. Qué tienes que hacer

Un formulario, tres minutos, el día de la entrega. Valoras a **tus dos compañeros y
también a ti mismo**.

**Primero**, una línea sobre qué hizo cada uno en el bloque, tú incluido. No es
relleno: obliga a recordar antes de puntuar, y es lo que evita que la valoración se
convierta en una impresión general.

**Después**, un nivel por persona:

| | Nivel | Qué significa |
|:-:|---|---|
| **A** | Implicación plena | Cumplió lo acordado en plazo, la calidad de su aportación fue la esperada y facilitó el trabajo de los demás. |
| **B** | Con altibajos | Cumplió en lo esencial, pero hubo retrasos o hubo que reclamarle alguna parte. |
| **C** | Insuficiente | Aportó menos de lo que le correspondía; otros asumieron parte de su trabajo. |
| **D** | Mínima o nula | Apenas participó. |

**Y por último**, solo si has marcado C o D para alguien: una línea diciendo qué
tendría que haber pasado para que fuera A.

Si todo ha ido bien, el formulario se acaba en tres clics y tres líneas. Ese es el
caso normal y está diseñado para ser el más rápido.

## 3. Cómo se combinan las valoraciones

Cada persona recibe **tres** valoraciones: las de sus dos compañeros y la suya propia.
De esas tres se toma la **mediana**, es decir, el valor central.

Esto tiene dos consecuencias que conviene ver con ejemplos, porque son la razón de que
el sistema sea justo:

**Una valoración aislada no te hunde.**

| Compañero 1 | Compañero 2 | Tú | Valores | Mediana | Resultado |
|:-:|:-:|:-:|:-:|:-:|---|
| A | D | A | 1, 4, 4 | **4** | La D queda fuera. Si uno solo dice algo distinto de lo que dicen los otros dos, no cambia nada. |

**Y tampoco puedes salvarte a ti mismo.**

| Compañero 1 | Compañero 2 | Tú | Valores | Mediana | Resultado |
|:-:|:-:|:-:|:-:|:-:|---|
| C | C | A | 2, 2, 4 | **2** | Tu voto es uno de tres: nunca gana contra los otros dos. |

Hacen falta **dos coincidencias** para mover el resultado. Ese es todo el mecanismo, y
es la razón de que tu autoevaluación cuente de verdad sin que puedas usarla para
inflarte.

## 4. Cómo afecta a tu nota

La mediana se traduce en un factor **w** que multiplica la nota del proyecto:

| Mediana | Nivel | **w** |
|:-:|---|:-:|
| 4 o 3 | A o B | **1,00** — nota íntegra del equipo |
| 2 | C | **0,75** |
| 1 | D | **entre 0 y 0,30**, fijado por la profesora con evidencia y en tutoría |

Fíjate en que **B no penaliza**. «Cumplió en lo esencial, con algún retraso» sigue
siendo cumplir, y w está para detectar a quien no hizo su parte, no para afinar la
calidad. B existe porque informa a la profesora de que un equipo tiene un problema de
proceso, no para restarte.

Y de ahí sale tu nota del estudio de caso:

$$\text{Gru} = \text{Gru}_0 \times w \qquad\qquad \text{EC} = 0{,}6 \cdot \text{Gru} + 0{,}4 \cdot \text{Ind}$$

donde $\text{Gru}_0$ es la nota del proyecto del equipo e $\text{Ind}$ la de tu examen
oral individual. Para superar la evaluación continua hacen falta las tres cosas:
$\text{Gru} \geq 3{,}5$, $\text{Ind} \geq 3{,}5$ y $\text{EC} \geq 5$.

### Un ejemplo completo

Equipo con un proyecto de **7,33**:

| | Recibe | Mediana | w | Gru | Ind | **EC** | |
|---|:-:|:-:|:-:|:-:|:-:|:-:|---|
| Ana | A, A + auto A | 4 | 1,00 | 7,33 | 8,1 | **7,64** | supera |
| Luis | C, C + auto B | 2 | 0,75 | 5,50 | 6,4 | **5,86** | supera |
| Sara | A, B + auto A | 4 | 1,00 | 7,33 | 5,2 | **6,48** | supera |

Los tres han hecho el mismo proyecto. Luis se lleva casi dos puntos menos porque sus
dos compañeros coincidieron en que no hizo su parte.

## 5. Tus garantías

**Lo que dices es confidencial.** Solo lo ve la profesora. Nadie sabrá nunca qué
valoración pusiste a quién.

**Lo que recibes también.** Se te comunica tu **w**, no las valoraciones concretas que
lo produjeron. No sabrás quién dijo qué sobre ti, ni siquiera si preguntas. Es la única
forma de que un equipo siga funcionando después.

**No hay represalias posibles.** Las valoraciones emitidas por quien acaba en el nivel
D **no se computan**. Si alguien intenta arrastrar a los demás para compensar su propia
situación, sus valoraciones se retiran del cálculo.

**Se cumplimenta antes de conocer ninguna nota**, el día de la entrega. No es un ajuste
de cuentas *a posteriori*.

**Si tu w no es 1, se te explica.** Y si acabas en el tramo D, no se cierra la nota sin
una tutoría en la que puedas dar tu versión.

## 6. Cómo hacerlo bien

- **Sé honesto, incluso contigo.** Poner A a todo el mundo por comodidad desactiva el
  instrumento y perjudica a quien sí trabajó.
- **Valora conductas, no simpatías.** «Entregó tarde tres veces» es valorable; «me cae
  regular» no.
- **Usa la línea de justificación.** Una C sin explicación es una acusación; con
  explicación es información útil.
- **No lo pactéis en grupo.** Si os ponéis de acuerdo en poneros todos A, habéis
  renunciado a la única herramienta que os protege del que no trabaja.
- **Si hay un problema, dilo en el bloque 1.** Hay tres coevaluaciones precisamente
  para poder intervenir a tiempo. Callar hasta la última no arregla nada.

---

# Parte 2 · Procedimiento del profesorado

*(Recortar antes de distribuir a los estudiantes.)*

## 7. Calendario por bloque

| Cuándo | Qué |
|---|---|
| Día de la entrega | Se abre el PR **y** se cumplimenta la coevaluación, antes de conocer ninguna nota |
| Semana siguiente | Corrección del informe en el PR |
| | Exposición y defensa |
| ≤ 2 semanas | Examen oral individual |
| Después | Ejecutar el agregador, cerrar w y publicar |

El orden importa: si la coevaluación se rellena después de conocer notas, deja de medir
implicación y pasa a medir descontento.

## 8. Operativa

**Recogida.** Formulario de Google generado por `coevaluacion_google_forms.gs`
(función `crearFormulario`). Una sección por equipo con los nombres precargados; el
estudiante elige equipo y responde sobre los tres.

**Agregación.** Función `agregarRespuestas` del mismo script. Escribe una hoja
*Resumen* con:

| Columna | Contenido |
|---|---|
| Valoraciones recibidas | Deben ser 3. Menos significa que alguien no respondió |
| Mediana provisional | Antes de aplicar la salvaguarda |
| Mediana final | Después de retirar los votos de quienes quedan en D |
| Factor w | 1,00 · 0,75 · o vacío si cae en el tramo docente |
| Detalle | Quién valoró qué |

> **La columna «Detalle» no sale nunca de tu Drive.** Es la única información del
> sistema cuya filtración destruiría el instrumento: en el bloque siguiente todo el
> mundo pondría A.

**Publicación.** La w entra en la columna *Co* de la hoja «Nota del proyecto» de
`Checklist_proyecto_equipo.xlsx`, que calcula Gru, EC y el cumplimiento de los tres
mínimos. El reparto lo hace `publicar_resultados.gs`: hoja de equipo con el retorno
común, y correo individual con w, Ind y EC.

## 9. El tramo D

Es el único que no se automatiza, y es deliberado: distinguir «hizo muy poco» de «no
hizo nada» no lo puede decidir un algoritmo con tres clics.

**Procedimiento:**

1. Reunir evidencia antes de hablar con nadie: historial de commits del repositorio,
   hilos del PR, bitácora en Notion, y las justificaciones de la Parte 3 del
   formulario.
2. Tutoría con el estudiante. Se le explica en qué banda ha quedado y por qué, y da su
   versión. No se comunican valoraciones individuales, solo el resultado agregado.
3. Fijar w entre 0 y 0,30 y dejarlo por escrito, con la evidencia en que se apoya.
4. Solo entonces, comunicar la nota.

**Por qué el suelo no puede ser 0,50.** Comprobado numéricamente sobre el rango
realista de notas (Gru₀ de 5 a 10, Ind de 3,5 a 10, 154 combinaciones):

| w | El polizón aprueba en |
|:-:|---|
| 0,50 | **59** combinaciones |
| 0,40 | 21 |
| **0,34** | **0** |
| 0 | 0 |

Con w = 0,50, un estudiante que no trabajó pero se estudia el informe y defiende bien
el oral supera la asignatura siempre que su equipo sea bueno — precisamente al revés de
lo que debería ocurrir. El techo de 0,30 deja margen dentro de la zona segura.

## 10. Límite conocido de la salvaguarda

Las exclusiones se calculan **en una sola pasada**: primero las medianas provisionales,
luego se retiran los votos de quienes quedan en D, y se recalcula una vez. No se itera
hasta un punto fijo.

Existe un caso en que eso importa: si quien cae en D era el único que sostenía a un
compañero, al retirar su voto ese compañero puede bajar de mediana 2 a 1,5 y caer al
tramo docente, mientras que sus propios votos ya han contado para los demás.

Es raro y describe un equipo roto, pero conviene reconocerlo. **La señal es que esa
fila sale con w vacío**: cualquier fila así exige mirarla, no cerrarla automáticamente.

No se itera a propósito: una cadena de exclusiones sería imposible de explicar a un
estudiante, y un procedimiento de calificación que no se puede explicar no se puede
sostener.

## 11. Uso formativo

El valor principal de la coevaluación del **bloque 1** no es calificar: es el aviso
temprano. Una mediana en C en la semana 5 deja diez semanas para intervenir.

Conviene mirar, además de la w:

- Equipos donde los tres se ponen A pero el historial de commits está muy desequilibrado
  — puede ser un pacto de no agresión. Ojo: el número de commits es un mal indicador por
  sí solo (quien redacta prosa genera diffs distintos de quien programa, y una sesión en
  pareja aparece como commits de una sola persona). Sirve para abrir una conversación,
  nunca para cerrarla.
- Equipos con varias B: hay fricción de proceso que aún se puede reconducir.
- Estudiantes que no responden: no cumplimentar la coevaluación limita w, y conviene
  advertirlo antes de aplicarlo.

## 12. Reclamaciones

Lo que se puede enseñar a un estudiante que reclama:

- Su mediana y su w.
- El número de valoraciones recibidas.
- El método completo — esta misma guía.
- En el tramo D, la evidencia documental (commits, PR, bitácora).

Lo que no, en ningún caso: **quién puso qué**. Si la reclamación se sostiene solo en
saberlo, la respuesta es que el sistema está diseñado para que eso no se comunique, y
que por eso mismo hacen falta dos coincidencias para que una valoración tenga efecto.
