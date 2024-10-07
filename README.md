# Descripción general

El lenguaje MoviScript es un lenguaje especifico de dominio (DSL) orientado a la programación de móviles como el recorrido de pequeños autos eléctricos programables y robots. Este lenguaje tiene la finalidad de facilitar la programación de la movilidad, ofreciendo una sintaxis intuitiva y funcionalidades específicas para abordar los desafíos asociados con estos dispositivos. Este DSL está diseñado para simplificar tareas comunes en la programación del desplazamiento, promoviendo un desarrollo eficiente.
Este lenguaje cuenta con comandos clasicos de un lenguaje imperativo, como la posiblidad de operar sobre expresiones matemáticas y booleanas, condicionales y bucles. Cuenta con instrucciones específicas de la programación de móviles como la posiblidad de generar generar una lista de puntos en función de una expresión matemática y una lista de puntos donde evaluarla.
Cuenta con instrucciones para el seguimiento de un camino (una lista de puntos), como así también la posiblidad de que, en caso de verse impedido el avance del móvil elabore un plan de contingencia para poder llegar al siguiente punto del camino.
La implementación de un lenguaje, el DSL se construye directamente embebido dentro de Haskell, utilizando sus funcionalidades como base. Luego, se emplea la biblioteca Parsec de Haskell para llevar a cabo el análisis sintáctico del código fuente del DSL.
Una vez completado el paso de parseo, se procede a la construcción de un Árbol de Sintaxis Abstracta (AST). Este árbol representa la estructura jerárquica del código fuente y sirve como una representación interna que facilita la manipulación y evaluación del código.
Finalmente, para la evaluación semántica del DSL, se recurre a un evaluador monádico. Las mónadas en Haskell ofrecen un mecanismo para modelar cómputos con efectos secundarios, lo que resulta esencial para la interpretación y ejecución de expresiones en nuestro lenguaje.

# Manual de uso

El lenguaje Movi esta embebido en Haskell por lo que para compilar el lenguaje MoviScript se deberá tener instalado un compilador de Haskell.
También es necesario tener instalado el programa ucblogo para poder ejecutar el script en logo con el que se evalua el programa. Para hacer estas intalaciones se puede ejecutar en una distribucion Debian o Ubuntu el siguiente comando:

``` {.bash language="bash"}
apt-get install ghci
    apt-get install ucblogo
```

Para ejecutar un programa MoviScript se debe ejecutar el script movi.sh en la misma carpeta en la que se encuentran los archivos movi.sh, Main.hs, Parser.hs y EvalMovi.hs:

``` {.bash language="bash"}
./movi.sh <ruta-del-programa>/miPrograma.movi 
```

De esta forma se ejecuta el programa $miPrograma.movi$. La ejecución en esta primer versión de MoviScript producirá una salida en pantalla con las primitivas ejecutadas por el móvil y la traza con los puntos por los que pasó el móvil y un gráfico de la traza realizado en logo.

# Expresiones flotantes

El lenguaje incorpora funciones trigonométricas (seno, coseno, tangente), logaritmo, redondeo hacia abajo (floor) y redondeo hacia arriba (ceil), ofreciendo así una amplia gama de herramientas matemáticas. La inclusión de funciones especializadas como la exponencial y el redondeo a un entero mediante la función 'round' brinda flexibilidad a los desarrolladores para expresar de manera eficiente y concisa operaciones matemáticas complejas.
Esta sintaxis proporciona una base robusta para la manipulación de cálculos numéricos en el contexto de programación de móviles. La sintaxis del lenguaje es simple y se describe a continuacion.

$$\textbf{ - } \textit{ expf }
\text{: Negaci\'on de una expresi\'on flotante (expf).}$$
$$\textbf{ ( } \textit{ expf }\textbf{ ) }\text{: Agrupaci\'on de una expresi\'on flotante.}$$
$$\textit{ expf } \textbf{ + } \textit{ expf }\text{: Suma de dos expresiones flotantes.}$$
$$\textit{ expf } \textbf{ * } \textit{ expf }\text{ Multiplicación de dos expresiones flotantes.}$$
$$\textit{ expf } \textbf{ - } \textit{ expf }\text{ Resta de dos expresiones flotantes.}$$
$$\textit{ expf } \textbf{ / } \textit{ expf }\text{ División de dos expresiones flotantes.}$$
$$\textbf{ sen ( } \textit{ expf } \textbf{ ) }\text{: Seno de una expresión flotante.}$$
$$\textbf{ cos ( } \textit{ expf } \textbf{ ) }\text{: Coseno de una expresión flotante.}$$
$$\textbf{ tan ( } \textit{ expf } \textbf{ ) }\text{' expf '): Tangente de una expresión flotante.}$$
$$\textbf{ log ( } \textit{ expf } \textbf{ ) }\text{: Logaritmo de base 10 una expresión flotante.}$$
$$\textbf{ floor ( } \textit{ expf } \textbf{ ) }\text{: Redondeo hacia abajo de una expresión flotante.}$$
$$\textbf{ ceil ( } \textit{ expf } \textbf{ ) }\text{: Redondeo hacia arriba de una expresión flotante.}$$
$$\textbf{ exp ( } \textit{ expf , expf } \textbf{ ) }\text{: Función exponencial con dos argumentos flotantes.}$$
$$\textbf{ round ( } \textit{ expf , num } \textbf{ ) }
\begin{aligned}
\text{Redondeo de una expresión flotante con una } \\
\text{cantidad determinada (num) de decimales.}
\end{aligned}$$

# Expresiones booleanas

Las expresiones booleanas se pueden relacionar con los siguientes operadores.

$$\textbf{true} \text{ y } \textbf{false}\text{: Representan valores booleanos directos.}$$
$$\textit{expf} < \textit{expf} \text{ y } \textit{expf} > \textit{expf}\text{: Permite comparaciones relacionales entre expresiones flotantes.}$$
$$\textit{boolexp } \& \textit{ boolexp}\text{: Operador lógico AND para conjunción de expresiones booleanas.}$$
$$\textit{boolexp } | \textit{ boolexp}\text{: Operador lógico OR para disyunción de expresiones booleanas.}$$
$$\sim \textit{boolexp}\text{: Operador unario NOT para negación de una expresión booleana.}$$
$$\textbf{(} \textit{ boolexp } \textbf{)}\text{: Permite agrupar expresiones booleanas para establecer el orden de evaluación.}$$

# Comandos básicos y de control de flujo

En este lenguaje, se emplean comandos fundamentales para la manipulación eficiente de variables y el control del flujo de ejecución típicos en cualquier lenguaje imperativo.
$$\textit{varf} \textbf{ := } \textit{ expf }$$ La asignacion de expresión flotante (expf) a una variable de tipo flotante (varf) se hará con el operador $:=$. En MoviScript sólo se permitirá el almacenamiento de valores flotantes en las variables. En versiones futuras se podrán incorporar variables de punto, de listas de expresiones flotantes y de listas de punto.
$$\textit{comm} \textbf{ ; } \textit{ comm }
    \text {: La secuenciaci\'on de comandos se realiza con el caracter ';'.}$$
$$\textbf{ if ( } \textit{boolexp} \textbf{) then } \textit{comm} \textbf{ else } \textit{comm} \textbf{ end }$$
Estructura condicional clasica if-then-else que ejecuta un bloque de comandos u otro según la evaluación de una expresión booleana.
$$\textbf{ while (} \textit{boolexp} \textbf{) do } \textit{comm} \textbf{ end }$$
Establece un bucle que repite la ejecución de un bloque de comandos mientras una condición booleana se mantenga verdadera.

# Comandos sobre puntos y listas de puntos

En MoviScript, las instrucciones relacionadas con puntos proporcionan herramientas fundamentales para la manipulación espacial y el análisis geométrico. Estas instrucciones permiten la opreaciones de puntos en un plano bidimensional como la medición de la distancia entre el origen y un punto específico, así como la determinación del ángulo que dicho
punto forma con el eje x. La capacidad de calcular la distancia y el ángulo facilita el desarrollo de algoritmos espaciales avanzados, permitiendo a los programadores abordar tareas relacionadas con la ubicación y la orientación en el contexto específico de dispositivos
móviles.
La representación de un punto en nuestro lenguaje sera un par ordenado de expresiones flotantes. $$point = ( expf , expf )$$ Sobre un punto se pueden realiar los siguientes comandos.
$$\textbf{dist(}point\textbf{)}
\text{. Establece la distancia entre un punto 
(point) y el origen de coordinadas}$$
$$\textbf{rangle(}point\textbf{)}$$ El comando $rangle$ establece el angulo (en radianes, de 0 a 2$\pi$) determinado entre la recta que pasa por el punto (point) y el eje x. $$\textbf{gangle(}point\textbf{)}$$ El comando $gangle$ establece el angulo (en grados, de 0 a 360°) determinado entre la recta que pasa por el punto (point) y el eje x.
$$\textbf{path(}expf , var, [expf] \textbf{)}$$
Con el comando $path$ podremos generar listas de puntos $(x_i,y_i)$ en función de una expresión matemática $(f)$, una variable $(x)$ y una lista de expresiones flotantes donde evaluaremos nuestra expresión matemática ($x_i$). El comando toma tres argumentos:

-   $expf:$ Es una expresión matemática ($f$) que generara los valores
    $y_i$.

-   $var:$ Es la variable $(x)$ en que se evaluará en la expresión
    matemática $expf$.

-   $[expf]$: Es la lista de valores $x_i$ en la que se evaluará la
    función $f$.

# Comandos de movimientos

En esta sección describiremos las funciones que brindan la capacidad del dispositivo para moverse de manera inteligente y eficiente, ofreciendo a los programadores las herramientas necesarias para la creación de algoritmos de movilidad. Por un lado tenemos herramientas de navegación básica: $$\textbf{fd(} dist , time \textbf{)}$$ Con el comando $Fd$ se indica al móvil el avance lineal de una distancia($dist$, de tipo expresión flotante) en durante en un tiempo determinado($time$, de tipo expresión flotante). $$\textbf{bk(} dist , time \textbf{)}$$ Con el comando $Bk$ se indica al móvil el retroceso lineal de una distancia($dist$, de tipo expresión flotante) en durante en un tiempo determinado($time$, de tipo expresión flotante).
$$\textbf{turn(} ang , time \textbf{)}$$ Con el comando $Turn$ se indica al móvil el giro de un angulo en grados($ang$, de tipo expresión flotante) en durante en un tiempo determinado($time$, de tipo expresión flotante).

El lenguaje, también cuenta con instrucciones complejas que permiten elaboración de algoritmos avanzados:
$$\textbf{lookat(}point, vel\textbf{)}$$ Con el comando $LookAt$ se indica al móvil el giro en dirección al punto ($point$). La coordenada del punto será en caracter relativo al propio móvil tomando como origen de coordenadas el móvil y el sentido positivo del eje x será la dirección en la que esta apuntando el móvil al momento de ejecutar la instrucción. La velocidad de giro será la indicada como $vel$.
$$\textbf{goline(}point , vel-lineal , vel-angular \textbf{)}$$ Con el comando $GoLine$ se indica al móvil el giro en dirección al punto ($point$) y luego su desplazamiento al punto mencionado. La coordenada del punto será en caracter relativo al propio móvil tomando como origen de coordenadas el móvil y el sentido positivo del eje x será la dirección en la que esta apuntando el móvil al momento de ejecutar la instrucción. La velocidad de giro será la indicada como $vel-angular$ y la velocidad de desplazamiento será la indicada como $vel-lienal$.

$$\textbf{follow(}listpoint , vel-lineal , vel-angular \textbf{)}$$ Con el comando $follow$ se indica que el móvil haga un recorrido determinado por la lista de puntos ($listpoint = [(x_1,y_1),(x_2,y_2)...(x_n,y_n)]$). La velocidad de giro será la indicada como $vel-angular$ y la velocidad de desplazamiento será la indicada como $vel-lienal$. Los puntos del camino están en
relación a al punto de referencia que es el móvil al momento de iniciarse la ejecución del programa.
$$\textbf{followsmart(}listpoint-allow , listpoint-contingency-allow , vel-lineal , vel-angular \textbf{)}$$
De igual manera que el comando $follow$, el comando $followsmart$ indica que el móvil haga un recorrido determinado por la lista de puntos seguido de las posiciones en las que estará habilitado el móvil para circular. La lista de puntos-contingency será la lista de puntos ($listpoint-allow = def\_obstacles [(x_1,y_1),(x_2,y_2)...(x_n,y_n)], [x_1 ... x_i ... x_n]$)
donde $x_i$ indica la lista de puntos en los que el móvil esta obstaculizado por el medio para ciruclar. En este caso, el comando $followsmart$ incorpora información del medio. Lo hace mediante la lista de puntos que en este caso será una lista de índices en los que los puntos obstaculizados por el medio. Si el móvil intentar circular por una parte del camino que está impedido por algún obstaculo el lenguaje MoviScript tendrá un camino de contingencia. Al igual que en el caso de $follow$, los puntos del camino están en relación a al punto de referencia que es el móvil al momento de iniciarse la ejecución del programa. Sin embargo, la lista de contingencia es relativa al último punto antes de encontrarse un obstaculo y se utilizará como contingencia de forma recursiva, de forma de que si hay obstaculos abordando el camino de contingencia el móvil también utilizará el camino de contingencia.

# Semántica de los comandos

**Lookat**
$$\frac{p \downarrow (p_x,p_y)}{Lookat(p, v) \to Turn( \frac{180} {\pi} * atan^2(p_x, p_y) , v)}$$

**Goline**
$$\frac{p \downarrow dist}{Goline (p,v_1,v_2) \to Seq( Lookat(p, v_1) , Fd (\frac{dist}{v_2}, v_2)}$$

**TurnAbs**
$$\frac{}{TurnAbs(ang, vel) \to Turn\left(\left(\frac{ang - angAct}{v}\right), vel\right)}$$

**GolineAbs**
$$\frac{}{ GolineAbs((p_x,p_y), v_1, v_2) \to Seq (TurnAbs \left(\frac{180}{\pi} \cdot atan2 (p_y, p_x)\right) \ v_1, Fd(\frac{dist}{v_2}), v_2)}$$

**Follow** $$\frac{}{Follow([], v_1, v_2) \to Skip}$$

$$\frac{}{Follow([p], v_1, v_2) \to GolineAbs(p, v_1, v_2)}$$

$$\frac{}{Follow(p:ps, v_1, v_2) \to {Seq(GolineAbs(p, v_1, v_2), Follow(ps, v_1, v_2))}}$$

$$\frac{Path (exp,v, list) \downarrow xs}{Follow(Path (exp,v, list), v_1, v_2) \to {Follow(xs, v_1, v_2)}}$$

**Followsmart**
$$\frac{}{FollowSmart([], contingency, v_1, v_2) \to Skip}$$

$$\begin{split}
    \frac{}{FollowSmart([(p,True):xs], contingency, v_1, v_2) \to}\\
     Seq ( GolineAbs(p, v_1, v_2), FollowSmart(xs, contingency, v_1, v_2))
        \end{split}$$

$$\frac{}{FollowSmart([(p,False):xs], contingency, v_1, v_2) \to Skip}$$

$$\frac{isAllFalse(contingency)}{FollowSmart([(p,False):xs], contingency, v_1, v_2) \to FollowSmart(xs, [], v_1, v_2)}$$

$$\frac{\neg isAllFalse(contingency)}{FollowSmart([(p,False):xs], contingency, v_1, v_2) \to
        FollowSmart(contingency \mathop{+\!\!+} xs, contingency, v_1, v_2) }$$

$$\begin{split}
\frac{def\_obstacles(ps,list) \downarrow xs}{FollowSmart(def\_obstacles(ps,list), contingency, v_1, v_2) \to FollowSmart(xs, contingency, v_1, v_2)}
\end{split}$$

$$\begin{split}
    \frac{def\_obstacles(ps,list) \downarrow xs}{FollowSmart(listpoints, def\_obstacles(ps,list), v_1, v_2) \to FollowSmart(listpoints, xs, v_1, v_2)}
    \end{split}$$

# Posibles próximas versiones

En futuras versiones, MoviScript podrá tomar información del medio con un sensor frontal del mḿovil. También habrá posiblidad de almacenamiento de más tipos de variables como las listas, los puntos y la lista de obstaculos. También se podrá compilar MoviScript a código $NQC$[^1] y de esta forma podrá ser ejecutado en un móvil construido en $LEGO MINDSTORMS$.

[^1]: Not Quite C es un lenguaje simple que sintaxis similar a C utilizado para programar la ínea robótica de LEGO denominada Maindstorms. Para más información visitar la documentación: https://bricxcc.sourceforge.net/nqc/