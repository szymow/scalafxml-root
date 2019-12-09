
package scalafxml.demo

import scalafx.event.ActionEvent
import scalafx.scene.control.{Button, Label, ListView, TextField}
import scalafxml.core.macros.sfxml

import scala.collection.mutable.Stack

case class TestDependency(initialPath: String)

@sfxml
class StartScreenPresenter(
														display: Label,
														display1: Label,
    newPhotoBookPath: TextField,
    btCreate: Button,
    recentPaths: ListView[String],
    testDep: TestDependency) {

  println(s"testDep is $testDep")

	newPhotoBookPath.text = testDep.initialPath

	var displayBufer: String = ""
	var display1Bufer: String = ""
	var znak : Char = '\u0000'
	var t : Char = '\u0000'

	var stosWartosci = Stack[Double]()
	var stosZnakow = Stack[Char]()

	val znaki = List('+','-','*','/')
	var pierwsza_liczba : Boolean = true
	var wyswietlono_wynik : Boolean = false
	var bladDziel : Boolean = false

	def processNumbers(event: ActionEvent) {
		println(newPhotoBookPath.text)
		val text : String = event.getSource.toString
		if (text.length == 36){
			t = '.'
		}
		else {
			t = text.charAt(35)
		}

		aktualizuj()

		if(t == '.' && displayBufer.isEmpty) displayBufer = "0t"
		if(!(t == '.' && displayBufer.contains("."))) displayBufer = displayBufer.concat(t.toString)

		display.text = displayBufer
	}

	def processOperators(event: ActionEvent) {
		println("Process Operators")
		val ops : String = event.getSource.toString
		print(ops)
		print(ops.length)
		ops.length match{
			case 50 => znak = 'z'
			case 37 => znak = ops.charAt(35)
				case _ => znak = ops.charAt(38)
		}
		print(znak)

		if (display1Bufer.contains('(') && znak == 'z') return
		if (displayBufer.isEmpty && display1Bufer.isEmpty && (znaki.contains(znak) || znak == ')')) return

		if(bladDziel){
			display1.text = ""
			display1Bufer = ""
			bladDziel = false
		}
		if(wyswietlono_wynik){
			display.text = ""
			displayBufer = ""
			wyswietlono_wynik = false
		}
		if(znak == 'C'){
			czyszczenie()
		}
		else{
			//Zapobiega wpisanu paru znakow zaraz po sobie
			if (displayBufer.isEmpty && !display1Bufer.isEmpty)
				if (znaki.contains(znak) && znaki.contains(display1Bufer.last)) return
			if (znak == 'z'){
				var temp : Double = displayBufer.toDouble
				temp = -temp
				displayBufer = temp.toString
				display.text = displayBufer
				return
			}
			else
			{
				display1Bufer = display1Bufer + displayBufer + znak
				display1.text = display1Bufer
			}
		}
		znak match {
			case '+'  | '-' =>
				sprawdz_nawias()
				display.text = ""
				displayBufer = ""
				if (!(pierwsza_liczba)){
					if(stosZnakow.top == '+' || stosZnakow.top == '-' || stosZnakow.top == '*' || stosZnakow.top == '/')
						do dzialanie()
						while(stosZnakow.nonEmpty && (stosZnakow.top == '+' || stosZnakow.top == '-'))
				}
				else pierwsza_liczba = false
				stosZnakow.push(znak)
			case '/' | '*' =>
				sprawdz_nawias()
				display.text = ""
				displayBufer = ""
				if (!(pierwsza_liczba)){
					if(stosZnakow.top == '*' || stosZnakow.top == '/')
						dzialanie()
				}
				else pierwsza_liczba = false
				stosZnakow.push(znak)
			case '=' =>
				sprawdz_nawias()
				display.text = ""
				displayBufer = ""
				do dzialanie()
				while(stosZnakow.nonEmpty)
				pozostale_obliczenia()
			case ')' =>
				if ((display1Bufer.count(_ == '(') > display1Bufer.count(_ == ')')))
					stosWartosci.push(displayBufer.toDouble)
				display.text = ""
				displayBufer = ""
				if(stosZnakow.top != '(')
				{
					do dzialanie()
					while(stosZnakow.top != '(')
				}
				stosZnakow.pop()
				wyswietl_log()
				if(stosWartosci.size == 1) pierwsza_liczba = true
			case '(' =>
				if(stosZnakow.nonEmpty && stosZnakow.top == '-') stosWartosci.push(displayBufer.toDouble)
				stosZnakow.push(znak)
		}
	}

	def wyswietl_log(){
		println("\t stosWartosci: " + stosWartosci)
		println("\t stosZnakow: " + stosZnakow)
	}

	def pozostale_obliczenia(){
		if(stosZnakow.isEmpty && stosWartosci.nonEmpty && stosWartosci.size > 1){
			do{
				wyswietl_log()
				display.text = stosWartosci.push(dodawanie(stosWartosci.pop(), stosWartosci.pop())).toString()
				wyswietl_log()
			}
			while(stosWartosci.isEmpty)
		}
	}

	def dzialanie(){
		if(stosZnakow.nonEmpty && stosWartosci.nonEmpty) {
			wyswietl_log()

			stosZnakow.pop match {
				case '+' => display.text = stosWartosci.push(dodawanie(stosWartosci.pop(), stosWartosci.pop())).toString()
				case '-' => display.text = stosWartosci.push(odejmowanie(stosWartosci.pop(), stosWartosci.pop())).toString()
				case '*' => display.text = stosWartosci.push(mnozenie(stosWartosci.pop(), stosWartosci.pop())).toString()
				case '/' => if(stosWartosci.top != 0.0)
					display.text = stosWartosci.push(dzielenie(stosWartosci.pop(), stosWartosci.pop())).toString()
				else bladDzielZero()
			}
			wyswietlono_wynik = true
			wyswietl_log()

			pozostale_obliczenia()
		}
	}

	//Zamiana kolejności itemów związana jest z kolejnością zdejmowania wartości ze stosu
	def dodawanie(zmienna1: Double, zmienna2: Double): Double = zmienna2 + zmienna1
	def odejmowanie(zmienna1: Double, zmienna2: Double): Double = zmienna2 - zmienna1
	def mnozenie(zmienna1: Double, zmienna2: Double): Double = zmienna1 * zmienna2
	def dzielenie(zmienna1: Double, zmienna2: Double) : Double = zmienna2 / zmienna1

	def czyszczenie(){
		display.text = ""
		display1.text = ""
		displayBufer = ""
		display1Bufer = ""
		stosWartosci.clear()
		stosZnakow.clear()
		pierwsza_liczba = true
	}

	def bladDzielZero(){
		display1.text = "Błąd dzielenia przez 0"
		display.text = ""
		displayBufer = ""
		stosWartosci.clear()
		stosZnakow.clear()
		pierwsza_liczba = true
		bladDziel = true
	}

	def sprawdz_nawias(){
		//Przed ostatni znak
		if(!display1Bufer.isEmpty && !displayBufer.isEmpty)
			if(!display1Bufer.takeRight(2).contains(')'))
				stosWartosci.push(displayBufer.toDouble)
	}

	def aktualizuj(){
		if(bladDziel){
			display1.text = ""
			display1Bufer = ""
			bladDziel = false
		}
		if(wyswietlono_wynik){
			display.text = ""
			displayBufer = ""
			wyswietlono_wynik = false
		}
		if(display1Bufer.contains("=")){
			czyszczenie()
		}
	}


	def onBrowseForOpen(event: ActionEvent) {
		println("Process Operators")
		println("onBrowseForOpen")
	}

	def onCreate(event: ActionEvent) {
		println("onCreate")
	}
}