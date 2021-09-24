package co.s4n.calnat
import scala.io.StdIn

object Main extends App {
    def leerInt(prompt:String):Int = {
        val s = StdIn.readLine(prompt)
        s.toInt
    }
    def esCero(nat:Nat) = nat match {
        case Cero()     => true
        case Suc(nat)   => false
    }
    def esMayorIgual(nat1:Nat, nat2:Nat):Boolean = nat1 match {
        case Cero() => nat2 match {
            case Cero() => true
            case _      => false
        }
        case Suc(pnat) => nat2 match {
            case Cero() => true
            case Suc(snat) => esMayorIgual(pnat, snat)
        }
    }
    def conIntANat(i:Int):Nat = {
        if (i == 0) new Cero
        else new Suc(conIntANat(i-1))
    }
    def imprimirNat(myNat:Nat):String = myNat match {
        case Cero()     => "Cero"
        case Suc(natp)   => "Suc("+imprimirNat(natp)+")"
    }
    def restaNat(nat1:Nat, nat2:Nat):Nat = {
        def iRestaNat(biggerNat:Nat):Nat = {
            biggerNat match{
                case Cero() => new Cero()
                case Suc(bnat) => new Suc(iRestaNat(bnat))
            }
        }
        (nat1, nat2) match{
            case (Suc(snat1), Suc(snat2)) => restaNat(snat1, snat2)
            case (Suc(snat1), Cero()) => iRestaNat(nat1)
        }
    }


    //Aplication
    val a = leerInt("Leer primer entero ")
    val b = leerInt("leer segundo entero ")

    val natA = conIntANat(a)
    val natB = conIntANat(b)

    if(esMayorIgual(natA, natB)){
        print(imprimirNat(restaNat(natA, natB)))
    }
    else print("No se puede hacer la operacion")
}
