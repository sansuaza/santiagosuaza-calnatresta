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
    def imprimirNat(nat:Nat):String = nat match {
        case Cero()     => "(Cero)"
        case Suc(nat)   => "Suc("+Sec.nat+")"
    }

    def restaNat(nat1:Nat, nat2:Nat):Nat = (nat1, nat2) match {
        case (Suc(nat), Suc(nat)) => restaNat(nat1.nat, nat2.nat)
        case (Suc(nat), Cero()) => iRestaNat(nat1)
        def iRestaNat(biggerNat:Nat):Nat = biggerNat match {
            case Cero() => new Cero()
            case Suc(snat) => new Suc(iRestaNat(biggerNat.nat))
        }
    }

    //Aplication
    val a = leerInt("Leer primer entero ")
    val b = leerInt("leer segundo entero ")

    val natA = conIntANat(a)
    val natB = conIntANat(b)

    if(esMayorIgual(natA, natB)){
        imprimirNat(restaNat(natA, natB))
    }
    else print("No se puede hacer la operacion")
}
