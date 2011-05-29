import org.apache.bcel._
import org.apache.bcel.classfile._
import org.apache.bcel.generic._

trait Pile {
  def init(size: Int)
  def getSize: Int
 
  def load(obj: java.lang.Object, idx: Int): java.lang.Object
  def store(obj: java.lang.Object, idx: Int): java.lang.Object
}

trait Piled {
  def setPile(pile: java.lang.Object)
  def setPileIdx(pileIdx: Int)
  
  def getPile: java.lang.Object
  def getPileIdx: Int
}

object Piles {

protected def getFldTypes(flds: Seq[java.lang.reflect.Field]) =
  for(fld <- flds)
    yield (fld, Type.getType(fld.getType), new ArrayType(Type.getType(fld.getType), 1))

protected def genFunc(clsName: String, cg: ClassGen, name: String, rt: Type, argNames: Array[String], args: Array[Type])(p: (InstructionList, InstructionFactory, MethodGen) => Unit) {
  val il = new InstructionList
  val fac = new InstructionFactory(cg)
  val mg = new MethodGen(Constants.ACC_PUBLIC, rt, args, argNames, name, clsName, il, cg.getConstantPool)
 
  p(il, fac, mg)
 
  mg.setMaxStack
  cg.addMethod(mg.getMethod)
  il.dispose
}

def genPile(cls: java.lang.Class[_]): java.lang.Class[_] = {
  val flds = cls.getDeclaredFields 
  val tflds = List(getFldTypes(flds):_*) 
  val pileName = "$pile$" + cls.getName 
  val cg = new ClassGen(pileName, "java.lang.Object", null, Constants.ACC_PUBLIC | Constants.ACC_SUPER, null)
  val cp = cg.getConstantPool
 
  cg.addInterface(classOf[Pile].getName)
 
  cg.addField((new FieldGen(Constants.ACC_PRIVATE, Type.INT, "size", cp)).getField)
 
  for((fld, t, at) <- tflds)
    cg.addField((new FieldGen(Constants.ACC_PUBLIC, at, "$" + fld.getName, cp)).getField)
 
  genFunc(pileName, cg, "init", Type.VOID, Array("size"), Array(Type.INT))((il, fac, mg) => {
    for((fld, t, at) <- tflds) {
      il.append(new ALOAD(0))
      il.append(new ILOAD(1))
      il.append(fac.createNewArray(t, 1))
      il.append(fac.createFieldAccess(pileName, "$" + fld.getName, at, Constants.PUTFIELD))
    }
    il.append(InstructionConstants.RETURN)
  })
 
  genFunc(pileName, cg, "getSize", Type.INT, Array[String](), Type.NO_ARGS)((il, fac, mg) => {
    il.append(new ALOAD(0))
    il.append(fac.createFieldAccess(pileName, "size", Type.INT, Constants.GETFIELD))
    il.append(new IRETURN)
  })
 
  def genPileFunc(name: String)(p: (InstructionList, InstructionFactory, MethodGen, Int) => Unit) =
    genFunc(pileName, cg, name, Type.OBJECT, Array("obj", "idx"), Array(Type.OBJECT, Type.INT))((il, fac, mg) => {
      val clst = Type.getType(cls)
      val lg = mg.addLocalVariable("_obj", clst, null, null)
      val _obj = lg.getIndex
      il.append(new ALOAD(1))
      il.append(fac.createCast(Type.OBJECT, clst))
      lg.setStart(il.append(new ASTORE(_obj)))
      p(il, fac, mg, _obj)
      il.append(new ALOAD(1))
      il.append(InstructionConstants.ARETURN)
    })
 
  genPileFunc("load")((il, fac, mg, _obj) => {
    for((fld, t, at) <- tflds) { // if fld.getName == "x") {
      il.append(new ALOAD(_obj))
      il.append(new ALOAD(0))
      il.append(fac.createFieldAccess(pileName, "$" + fld.getName, at, Constants.GETFIELD))
      il.append(new ILOAD(2))
      il.append(InstructionFactory.createArrayLoad(t))
     
      scala.util.control.Exception.ignoring(classOf[java.lang.NoSuchMethodException]) {
        cls.getMethod(fld.getName + "_$eq", classOf[Int])
      }
      match {
        case setter: java.lang.reflect.Method => {
          il.append(fac.createInvoke(cls.getName, setter.getName, Type.getType(setter.getReturnType), Array[Type](t), Constants.INVOKEVIRTUAL))
          if(setter.getReturnType != classOf[Unit])
            il.append(new POP)
        }
        case _ => {
          il.append(fac.createFieldAccess(cls.getName, fld.getName, t, Constants.PUTFIELD))
        }
      }
    }
  })
  
  genPileFunc("store")((il, fac, mg, _obj) => {
    for((fld, t, at) <- tflds) { // if fld.getName == "x") {
      il.append(new ALOAD(0))
      il.append(fac.createFieldAccess(pileName, "$" + fld.getName, at, Constants.GETFIELD))
      il.append(new ILOAD(2))
      il.append(new ALOAD(_obj))
     
      scala.util.control.Exception.ignoring(classOf[java.lang.NoSuchMethodException]) {
        cls.getMethod(fld.getName)
      }
      match {
        case getter: java.lang.reflect.Method => {
          il.append(fac.createInvoke(cls.getName, getter.getName, Type.getType(getter.getReturnType), Type.NO_ARGS, Constants.INVOKEVIRTUAL))
        }
        case _ => {
          il.append(fac.createFieldAccess(cls.getName, fld.getName, t, Constants.GETFIELD))
        }
      }
     
      il.append(InstructionFactory.createArrayStore(t))
    }
  })
 
  cg.addEmptyConstructor(Constants.ACC_PUBLIC) 
  cg.getJavaClass.dump(pileName + ".class") 
  Class.forName(pileName)
}
 
def genPiled(cls: java.lang.Class[_], pileCls: java.lang.Class[_]) = {
  val flds = cls.getDeclaredFields 
  val tflds = List(getFldTypes(flds):_*)
  val pileType = Type.getType(pileCls)
  val piledName = "$piled$" + cls.getName 
  val cg = new ClassGen(piledName, cls.getName, null, Constants.ACC_PUBLIC | Constants.ACC_SUPER, null)
  val cp = cg.getConstantPool
 
  cg.addInterface(classOf[Piled].getName)
 
  cg.addField((new FieldGen(Constants.ACC_PRIVATE, pileType, "$pile", cp)).getField)
  cg.addField((new FieldGen(Constants.ACC_PRIVATE, Type.INT, "$pileIdx", cp)).getField)
 
  genFunc(piledName, cg, "setPile", Type.VOID, Array("pile"), Array(Type.OBJECT))((il, fac, mg) => {
    il.append(new ALOAD(0))
    il.append(new ALOAD(1))
    il.append(fac.createCast(Type.OBJECT, pileType))
    il.append(fac.createFieldAccess(piledName, "$pile", pileType, Constants.PUTFIELD))
    il.append(new RETURN)
  })
 
  genFunc(piledName, cg, "setPileIdx", Type.VOID, Array("pileIdx"), Array(Type.INT))((il, fac, mg) => {
    il.append(new ALOAD(0))
    il.append(new ILOAD(1))
    il.append(fac.createFieldAccess(piledName, "$pileIdx", Type.INT, Constants.PUTFIELD))
    il.append(new RETURN)
  })
 
  genFunc(piledName, cg, "getPile", Type.OBJECT, Array[String](), Type.NO_ARGS)((il, fac, mg) => {
    il.append(new ALOAD(0))
    il.append(fac.createFieldAccess(piledName, "$pile", pileType, Constants.GETFIELD))
    il.append(fac.createCast(pileType, Type.OBJECT))
    il.append(new ARETURN)
  })
 
  genFunc(piledName, cg, "getPileIdx", Type.INT, Array[String](), Type.NO_ARGS)((il, fac, mg) => {
    il.append(new ALOAD(0))
    il.append(fac.createFieldAccess(piledName, "$pileIdx", Type.INT, Constants.GETFIELD))
    il.append(new IRETURN)
  })
 
  def loadPileFld(il: InstructionList, fac: InstructionFactory, fld: java.lang.reflect.Field, t: Type, at: ArrayType) {
    il.append(new ALOAD(0))
    il.append(fac.createFieldAccess(piledName, "$pile", pileType, Constants.GETFIELD))
    il.append(fac.createFieldAccess(pileCls.getName, "$" + fld.getName, at, Constants.GETFIELD))
    il.append(new ALOAD(0))
    il.append(fac.createFieldAccess(piledName, "$pileIdx", Type.INT, Constants.GETFIELD))
  }
     
  for((fld, t, at) <- tflds) {
    genFunc(piledName, cg, fld.getName, t, Array[String](), Type.NO_ARGS)((il, fac, mg) => {
      loadPileFld(il, fac, fld, t, at)
      il.append(InstructionFactory.createArrayLoad(t))
      il.append(InstructionFactory.createReturn(t))
    })
   
    genFunc(piledName, cg, fld.getName + "_$eq", Type.VOID, Array("v"), Array(t))((il, fac, mg) => {
      loadPileFld(il, fac, fld, t, at)
      il.append(InstructionFactory.createLoad(t, 1))
      il.append(InstructionFactory.createArrayStore(t))
      il.append(new RETURN)
    })
  }
 
  cg.addEmptyConstructor(Constants.ACC_PUBLIC) 
  cg.getJavaClass.dump(piledName + ".class") 
  Class.forName(piledName)
}

}
