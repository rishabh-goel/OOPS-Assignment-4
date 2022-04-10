package com.rishabh.hw4

import com.rishabh.hw4.Computation.SetExp.{AbstractClassDef, Assign, ClassDef, Constructor, CreateMethod, Cross, ExceptionClassDef, Field, Interface, InvokeObject, NewObject, Params, Private, Public, Union, Value, Variable}

import java.lang
import scala.collection.{immutable, mutable}
import scala.collection.mutable.*

// Class created to mutate the value of isExceptionThrown field
class Computation {
  val isExceptionThrown = false
}

object Computation:

  // Creating an instance of Computation class to access its field
  val instance = new Computation()

  // private variable created to store and return the output of many exceptions being thrown
  private var output: BasicType = null

  import SetExp.*

  // Aliasing 'Any' to avoid hardcoding of Variable types
  type BasicType = Any

  // Map to store the macros
  val macroMap: scala.collection.mutable.Map[BasicType, SetExp] = scala.collection.mutable.Map()

  // Map to store classes, variables and scopes
  val scopeMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to identify which class has which object
  val objectMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store attributes an object can access
  val attrMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store class inheritance(child -> parent)
  val inheritanceMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store which class implements which interface
  val interfaceMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to store inheritance(outer -> inner)
  val nestedClassMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

  // Map to monitor access of fields and methods
  val accessMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map("public" -> scala.collection.mutable.Map(), "private" -> scala.collection.mutable.Map(), "protected" -> scala.collection.mutable.Map())

  // Map to store interfaces
  val dataStructureList: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map("class" -> scala.collection.mutable.ListBuffer(), "interface" -> scala.collection.mutable.ListBuffer(), "abstract_class" -> scala.collection.mutable.ListBuffer(), "exception_class" -> scala.collection.mutable.ListBuffer())

  enum SetExp:
    case Value(input: BasicType) // Get the value of the element passed
    case Variable(name: String) // Fetch the value assigned to the variable
    case Check(list: SetExp, item: SetExp) // Check if item present in set
    case Assign(name: String, item: SetExp) // Assign value to a variable
    case Insert(set: SetExp, item: SetExp) // Insert an item into the set
    case Delete(set: SetExp, item: SetExp) // Delete an item from the set
    case Union(set1: SetExp, set2: SetExp) // Union of 2 sets
    case Intersect(set1: SetExp, set2: SetExp) // Intersection of 2 sets
    case Diff(set1: SetExp, set2: SetExp) // Difference of 2 sets
    case Cross(set1: SetExp, set2: SetExp) // Cartesian Product of 2 sets
    case SetMacro(macroName: String, op: SetExp) // Create a macro in macroMap Map
    case GetMacro(macroName: String) // Fetch a macro from macroMap Map
    case Scope(name: String, op: SetExp*) // Set Scope of variables
    case ClassDef(className: String, expr: SetExp*) // Create Class Definition
    case InnerClass(outerClass: String, innerClass: String) // Define Outer and Inner classes
    case Field(name: String, expr: SetExp*) // Create field of class
    case Constructor(expr: SetExp) // Create constructor of class
    case NewObject(className: String, expr: SetExp, params: Params, values: ListBuffer[SetExp], parentObj: String*) // Create object of class
    case InvokeObject(className: SetExp, objectName: SetExp, attrName: SetExp, actualParams: SetExp*) // Access class attribute using an object
    case CreateMethod(methodName: String, params: Params, methodType: SetExp*) // Create method of a class
    case InvokeMethod(methodName: ListBuffer[SetExp], formalparam: SetExp, actualparam: Any) // Invoke method of a class
    case Public(param: SetExp) // Create Public access modifier
    case Private(param: SetExp) // Create Private access modifier
    case Protected(param: SetExp) // Create Protected access modifier
    case Params(parameters: String*) // Stores formal parameters for Constructors and Methods
    case AbstractClassDef(className: String, expr: SetExp*) // Create Abstract class
    case Interface(interfaceName: String, expr: SetExp*) // Create Interface
    case InnerInterface(outerInterface: String, innerInterface: String) // Create Inner Interface
    case If(condition: SetExp, thenClause: SetExp, elseClause: SetExp) // If condition construct
    case Then(clauses: SetExp*) // Construct executed when If condition results to true
    case Else(clauses: SetExp*) // Construct executed when If condition results to false
    case ThrowException(className: SetExp, excpMsg: String) // Construct to throw an exception
    case CatchException(className: SetExp) // Construct to catch the thrown exception
    case ExceptionClassDef(className: String, excpVar: SetExp*) // Exception class


    // Method to find the name of the class/abstract class/interface
    def findName(name: mutable.Map[BasicType, BasicType]): String = {
      val objName = scopeMap.find(_._2 == name).map(_._1) match {
        case Some(m) => m.asInstanceOf[String]
        case None => throw new Error("Item could not be found")
      }

      objName
    }

    // Method to update the access specifier map for public/protected/private accesses
    def getNewModifiers(modifier: String, parentName: String, childName: String): mutable.Map[BasicType, BasicType] = {
      // Get specified access members of parent and child references and create a new map for child reference
      val modifierParentMap = accessMap(modifier).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](parentName).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
      val modifierChildMap = accessMap(modifier).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](childName).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
      val newModifierChildMap = modifierChildMap.++(modifierParentMap)

      newModifierChildMap
    }

    // Method to add functionality of an interface to the class/abstract class
    def Implements(interface: SetExp) = {
      // Get elements of interface
      val parent = interface.eval().asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()

      // Get elements of class
      val child = this.eval().asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()

      // Get parent and child name
      val childName = findName(child)
      val parentName = findName(parent)

      // Identify the type of child reference that will be implementing the parent
      val childType = dataStructureList.find(_._2.asInstanceOf[scala.collection.mutable.ListBuffer[BasicType]].contains(childName)).map(_._1) match {
        case Some(m) =>
          m.asInstanceOf[String] match {
            case "class" => "class"
            case "abstract_class" => "abstract_class"
            case "interface" => {
              ExceptionClassDef("InterfaceInvalidTypeException", Field("InterfaceTypeReason")).eval()
              ThrowException(ExceptionClassDef("InterfaceInvalidTypeException"), "Only a class/abstract_class can implement an interface").eval()
              CatchException(ExceptionClassDef("InterfaceInvalidTypeException")).eval()
            }
          }

        case None => {
          // Using our own language's Try-Catch instead of Scala's try-catch
          ExceptionClassDef("InvalidTypeException", Field("InvalidTypeReason")).eval()
          ThrowException(ExceptionClassDef("InvalidTypeException"), "Invalid type").eval()
          CatchException(ExceptionClassDef("InvalidTypeException")).eval()
        }
      }

      // Identify the type of parent reference being used
      val parentType = dataStructureList.find(_._2.asInstanceOf[scala.collection.mutable.ListBuffer[BasicType]].contains(parentName)).map(_._1) match {
        case Some(m) =>
          m.asInstanceOf[String] match {
            case "interface" => "interface"
            case _ => {
              // Using our own language's Try-Catch instead of Scala's try-catch
              ExceptionClassDef("ClassInvalidTypeException", Field("ClassTypeReason")).eval()
              ThrowException(ExceptionClassDef("ClassInvalidTypeException"), "A class can't be implemented. It can only be extended").eval()
              CatchException(ExceptionClassDef("ClassInvalidTypeException")).eval()
            }
          }

        case None => {
          // Using our own language's Try-Catch instead of Scala's try-catch
          ExceptionClassDef("InvalidTypeException", Field("InvalidTypeReason")).eval()
          ThrowException(ExceptionClassDef("InvalidTypeException"), "Invalid type").eval()
          CatchException(ExceptionClassDef("InvalidTypeException")).eval()
        }
      }

      // Identify the methods present in both parent and child
      val parentKeys = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet
      val childKeys = child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet
      val commonKeys = parentKeys.intersect(childKeys)

      // If interface has more methods than class, then it should result in error as class has to implement all interface methods
      if(childType.equals("class") && parentType.equals("interface") && parentKeys.size > childKeys.size) {
        // Using our own language's Try-Catch instead of Scala's try-catch
        ExceptionClassDef("ClassImplementException", Field("ClassImplementReason")).eval()
        ThrowException(ExceptionClassDef("ClassImplementException"), "Class has to implement interface methods").eval()
        CatchException(ExceptionClassDef("ClassImplementException")).eval()
      }

      // Check if the parameter list for each method is same. If not, then throw an error
      commonKeys.foreach(key => {
        val parentMethod = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](key).asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]
        val childMethod = child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](key).asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]

        if(parentMethod.head != childMethod.head) {
          // Using our own language's Try-Catch instead of Scala's try-catch
          ExceptionClassDef("ParameterException", Field("ParameterMismatchReason")).eval()
          ThrowException(ExceptionClassDef("ParameterException"), "Cannot implement method from interface as method parameters don't match").eval()
          CatchException(ExceptionClassDef("ParameterException")).eval()
        }
      })

      // Check if any of the Exceptions as part of this code block has been thrown
      if(Variable("msgInterfaceInvalidTypeException").eval() == "msgInterfaceInvalidTypeException" &&
        Variable("msgInvalidTypeException").eval() == "msgInvalidTypeException" &&
        Variable("msgClassInvalidTypeException").eval() == "msgClassInvalidTypeException" &&
        Variable("msgClassImplementException").eval() == "msgClassImplementException" &&
        Variable("msgParameterException").eval() == "msgParameterException")
      {
        val methodMap = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone().++(child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]).clone()

        val newpublicChildMap = getNewModifiers("public", parentName, childName)

        // Update the access modifiers for child class after inheriting from the parent class
        accessMap.update("public", accessMap("public").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (childName -> newpublicChildMap))

        // keep track of which class inherits which class to avoid multiple inheritance
        interfaceMap += (childName -> parentName)

        // Map with updated values to be put again to the scopeMap
        val map: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map("field"-> child("field").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]], "method" -> methodMap, "constructor" -> child("constructor").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]])
        scopeMap += (childName -> map)
      }
      else {
        // Check which exception has been caught and return the error message
        // Fetching the output and removing the error message from the scope so it is not used by any other execution statement
        if(Variable("msgInterfaceInvalidTypeException").eval() != "msgInterfaceInvalidTypeException")
        {
          output = Variable("msgInterfaceInvalidTypeException").eval()
          scopeMap.remove("msgInterfaceInvalidTypeException")
        }
        else if(Variable("msgInvalidTypeException").eval() != "msgInvalidTypeException")
        {
          output = Variable("msgInvalidTypeException").eval()
          scopeMap.remove("msgInvalidTypeException")
        }
        else if(Variable("msgClassInvalidTypeException").eval() != "msgClassInvalidTypeException")
        {
          output = Variable("msgClassInvalidTypeException").eval()
          scopeMap.remove("msgClassInvalidTypeException")
        }
        else if(Variable("msgClassImplementException").eval() != "msgClassImplementException")
        {
          output = Variable("msgClassImplementException").eval()
          scopeMap.remove("msgClassImplementException")
        }
        else if(Variable("msgParameterException").eval() != "msgParameterException")
        {
          output = Variable("msgParameterException").eval()
          scopeMap.remove("msgParameterException")
        }

        output
      }
    }


    // Infix method to provide inheritance
    def Extends(superClass: SetExp) = {
      // Get elements of parent class
      val parent = superClass.eval().asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()

      // Get elements of child class
      val child = this.eval().asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()

      // Get parent and child name
      val childName = findName(child)
      val parentName = findName(parent)

      // Check if child and parent class are same
      if(parentName == childName) {
        // Using our own language's Try-Catch instead of Scala's try-catch
        ExceptionClassDef("SameInheritanceException", Field("SameInheritanceReason")).eval()
        ThrowException(ExceptionClassDef("SameInheritanceException"), "A class/interface cannot inherit itself").eval()
        CatchException(ExceptionClassDef("SameInheritanceException")).eval()
      }

      // Check if child class already inherits from some parent class
      if(inheritanceMap.contains(childName)) {
        // Using our own language's Try-Catch instead of Scala's try-catch
        ExceptionClassDef("MultipleInheritanceException", Field("MultipleInheritanceReason")).eval()
        ThrowException(ExceptionClassDef("MultipleInheritanceException"), "Cannot support multiple inheritance").eval()
        CatchException(ExceptionClassDef("MultipleInheritanceException")).eval()

        // Check which type of exception has been caught and return the error message
        if(Variable("msgSameInheritanceException").eval() != "msgSameInheritanceException")
        {
          val output = Variable("msgSameInheritanceException").eval()
          scopeMap.remove("msgSameInheritanceException")
          output
        }
        else
        {
          val output = Variable("msgMultipleInheritanceException").eval()
          scopeMap.remove("msgMultipleInheritanceException")
          output
        }
      } else {
        // Get private access members of parent class
        val privateMembers = accessMap("private").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](parentName)

        // Identify the type of child reference that will be extending the parent
        val childType = dataStructureList.find(_._2.asInstanceOf[scala.collection.mutable.ListBuffer[BasicType]].contains(childName)).map(_._1) match {
          case Some(m) =>
            m.asInstanceOf[String] match {
              case "class" => "class"
              case "abstract_class" => "abstract_class"
              case "interface" => "interface"
            }

          case None => {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("InvalidTypeException", Field("InvalidTypeReason")).eval()
            ThrowException(ExceptionClassDef("InvalidTypeException"), "Invalid type").eval()
            CatchException(ExceptionClassDef("InvalidTypeException")).eval()
          }
        }

        // Identify the type of parent reference that will be extended
        val parentType = dataStructureList.find(_._2.asInstanceOf[scala.collection.mutable.ListBuffer[BasicType]].contains(parentName)).map(_._1) match {
          case Some(m) =>
            m.asInstanceOf[String] match {
              case "class" => "class"
              case "abstract_class" => "abstract_class"
              case "interface" => "interface"
            }

          case None => {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("InvalidTypeException", Field("InvalidTypeReason")).eval()
            ThrowException(ExceptionClassDef("InvalidTypeException"), "Invalid type").eval()
            CatchException(ExceptionClassDef("InvalidTypeException")).eval()
          }
        }


        if(!parentType.equals("interface")){
          // Create new Constructor, Field and Method maps by first merging members of parent and child class
          // Then removing private members from fields and methods as they are not inherited
          val constructorMap = child.get("constructor") match {
            case Some(m) => parent("constructor").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].++(m.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]])
            case None => parent("constructor").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
          }

          val parentFieldMap = parent("field").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()
          privateMembers.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet.foreach(i => {
            parentFieldMap -= i
          })

          val fieldMap = child.get("field") match {
            case Some(m) => parentFieldMap.++(m.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]])
            case None => parentFieldMap
          }

          val parentMethodMap = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()
          privateMembers.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet.foreach(i => {
            parentMethodMap -= i
          })

          val methodMap = parentMethodMap.++(child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]])

          val parentKeys = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet
          val childKeys = child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet
          val commonKeys = parentKeys.intersect(childKeys)

          // Concrete class need to implement abstract method of abstract class
          if(commonKeys.isEmpty && childType.equals("class") && parentType.equals("abstract_class")) {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("IncompleteImplementationException", Field("IncompleteImplementationReason")).eval()
            ThrowException(ExceptionClassDef("IncompleteImplementationException"), "Abstract class method not implemented").eval()
            CatchException(ExceptionClassDef("IncompleteImplementationException")).eval()
          }

          // Check if the parameter list for each method is same. If not, then throw an error
          commonKeys.foreach(key => {
            val parentMethod = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](key).asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]
            val childMethod = child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](key).asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]

            if(parentMethod.head != childMethod.head) {
              // Using our own language's Try-Catch instead of Scala's try-catch
              ExceptionClassDef("ParameterException", Field("ParameterMismatchReason")).eval()
              ThrowException(ExceptionClassDef("ParameterException"), "Cannot override method as paramter list doesn't match").eval()
              CatchException(ExceptionClassDef("ParameterException")).eval()
            }
          })

          // Check if any of the exceptions defined in the code block have been caught or not
          if(Variable("msgInvalidTypeException").eval() == "msgInvalidTypeException" &&
            Variable("msgIncompleteImplementationException").eval() == "msgIncompleteImplementationException" &&
            Variable("msgParameterException").eval() == "msgParameterException")
          {
            // Get updated maps for the child
            val newpublicChildMap = getNewModifiers("public", parentName, childName)
            val newprotectedChildMap = getNewModifiers("protected", parentName, childName)

            // Update the access modifiers for child class after inheriting from the parent class
            accessMap.update("public", accessMap("public").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (childName -> newpublicChildMap))
            accessMap.update("protected", accessMap("protected").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (childName -> newprotectedChildMap))

            // keep track of which class inherits which class to avoid multiple inheritance
            inheritanceMap += (childName -> parentName)

            // Map with updated values to be put again to the scopeMap
            val map: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map("field" -> fieldMap, "constructor" -> constructorMap, "method" -> methodMap)
            scopeMap += (childName -> map)
          }
          // Check which type of exception has been caught and return the error message
          else if(Variable("msgInvalidTypeException").eval() != "msgInvalidTypeException")
          {
            output = Variable("msgInvalidTypeException").eval()
            scopeMap.remove("msgInvalidTypeException")
          }
          else if(Variable("msgIncompleteImplementationException").eval() != "msgIncompleteImplementationException")
          {
            output = Variable("msgIncompleteImplementationException").eval()
            scopeMap.remove("msgIncompleteImplementationException")
          }
          else if(Variable("msgParameterException").eval() != "msgParameterException")
          {
            output = Variable("msgParameterException").eval()
            scopeMap.remove("msgParameterException")
          }
          output
        }
        else {

          // Create new Method map by first merging members of parent and child interface
          // Then removing private methods as they are not inherited
          val parentMethodMap = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].clone()
          privateMembers.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet.foreach(i => {
            parentMethodMap -= i
          })

          val methodMap = parentMethodMap.++(child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]])

          val parentKeys = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet
          val childKeys = child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]].keySet
          val commonKeys = parentKeys.intersect(childKeys)

          if(commonKeys.isEmpty && childType.equals("class")) {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("IncompleteImplementationException", Field("IncompleteImplementationReason")).eval()
            ThrowException(ExceptionClassDef("IncompleteImplementationException"), "Abstract class method not implemented").eval()
            CatchException(ExceptionClassDef("IncompleteImplementationException")).eval()
          }

          // Check if the parameter list for each method is same. If not, then throw an error
          commonKeys.foreach(key => {
            val parentMethod = parent("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](key).asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]
            val childMethod = child("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]](key).asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]

            if(parentMethod.head != childMethod.head) {
              // Using our own language's Try-Catch instead of Scala's try-catch
              ExceptionClassDef("ParameterException", Field("ParameterMismatchReason")).eval()
              ThrowException(ExceptionClassDef("ParameterException"), "Cannot override method as paramter list doesn't match").eval()
              CatchException(ExceptionClassDef("ParameterException")).eval()
            }
          })

          // Check which type of exception defined in the code block has been thrown
          if(Variable("msgInvalidTypeException").eval() == "msgInvalidTypeException" &&
            Variable("msgIncompleteImplementationException").eval() == "msgIncompleteImplementationException" &&
            Variable("msgParameterException").eval() == "msgParameterException")
          {
            // Get updated maps for the child
            val newpublicChildMap = getNewModifiers("public", parentName, childName)
            val newprotectedChildMap = getNewModifiers("protected", parentName, childName)

            // Update the access modifiers for child interface after inheriting from the parent interface
            accessMap.update("public", accessMap("public").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (childName -> newpublicChildMap))
            accessMap.update("protected", accessMap("protected").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (childName -> newprotectedChildMap))

            // keep track of which interface inherits which interface to avoid multiple inheritance
            inheritanceMap += (childName -> parentName)

            // Map with updated values to be put again to the scopeMap
            val map: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map("method" -> methodMap)
            scopeMap += (childName -> map)
          }
          // Check if any of the exception has been caught and return the error message
          else if(Variable("msgInvalidTypeException").eval() != "msgInvalidTypeException")
          {
            output = Variable("msgInvalidTypeException").eval()
            scopeMap.remove("msgInvalidTypeException")
          }
          else if(Variable("msgIncompleteImplementationException").eval() != "msgIncompleteImplementationException")
          {
            output = Variable("msgIncompleteImplementationException").eval()
            scopeMap.remove("msgIncompleteImplementationException")
          }
          else if(Variable("msgParameterException").eval() != "msgParameterException")
          {
            output = Variable("msgParameterException").eval()
            scopeMap.remove("msgParameterException")
          }
          output
        }
      }
    }

    // Method where the execution of our program begins
    def eval(scope: scala.collection.mutable.Map[BasicType, BasicType] = scopeMap, access: scala.collection.mutable.Map[BasicType, BasicType]*): BasicType =

      // Helper method to create Inner Class/Inner Interface
      def createInnerRefHelper(innerRefName: String, outerRefName: String, innerRefType: String, currentScope: mutable.Map[BasicType, BasicType]) = {

        // Get elements of inner and outer reference from the current scope
        val inner = currentScope(innerRefName).asInstanceOf[scala.collection.mutable.Map[String, Any]]
        val outer = currentScope(outerRefName).asInstanceOf[scala.collection.mutable.Map[String, Any]]

        // Allow only 1 nested item to get created
        if(nestedClassMap.contains(outer)) {
          // Using our own language's Try-Catch instead of Scala's try-catch
          ExceptionClassDef("NestedException", Field("NestedClassReason")).eval(scope)
          ThrowException(ExceptionClassDef("NestedException"), "Cannot create multiple nested items").eval(scope)
          val output = CatchException(ExceptionClassDef("NestedException")).eval(scope)
          scope.remove("msgNestedException")
          output
        }
        else{
          // Update the map of outer item to include details of inner item
          outer += (innerRefType -> scala.collection.mutable.Map(innerRefName -> inner))
          nestedClassMap += (outerRefName -> innerRefName)

          // Remove the inner item from current scope as it has been moved to inside outer item
          currentScope.remove(innerRefName)
        }
      }


      // Helper method to create  Class/Abstract Class/Inner Interface
      def createRefHelper(itemName: String, itemType: String, currentScope: mutable.Map[BasicType, BasicType], expr: SetExp*) = {
        // If class is already created, return the elements of the item
        if (currentScope.contains(itemName))
          currentScope(itemName)
        else {

          // Create map to store fields, methods and constructors for a class
          val map: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()

          // Add field and constructor keys to the map only if we are not creating the interface
          if(!itemType.equals("interface")) {
            val fieldMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
            val constructorMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()
            map += ("field" -> fieldMap)
            map += ("constructor" -> constructorMap)
          }

          val methodMap: scala.collection.mutable.Map[String, ListBuffer[SetExp]] = scala.collection.mutable.Map()
          map += ("method" -> methodMap)

          // Create access modifier maps
          val publicClassMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map(itemName -> scala.collection.mutable.Map())
          val privateClassMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map(itemName -> scala.collection.mutable.Map())
          val protectedClassMap: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map(itemName -> scala.collection.mutable.Map())

          // Evaluate each expression inside class def with the appropriate scope
          expr.foreach(i => {
            i.eval(map, publicClassMap, privateClassMap, protectedClassMap)
          })

          // Check if the methods of the interface have body or not
          if(itemType.equals("interface")) {
            val interfaceMethods: scala.collection.mutable.Map[BasicType, BasicType] = map("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
            interfaceMethods.foreach(method => {
              val methodBody = method._2.asInstanceOf[scala.collection.mutable.ListBuffer[SetExp]]
              if (methodBody.size > 1) {
                // Using our own language's Try-Catch instead of Scala's try-catch
                ExceptionClassDef("InterfaceException", Field("Reason")).eval(scope)
                ThrowException(ExceptionClassDef("InterfaceException"), "Interface methods can't have body").eval(scope)
                CatchException(ExceptionClassDef("InterfaceException")).eval(scope)
              }
            })
          }

          // The items should be added to the maps only if no exception has been caught
          if(Variable("msgInterfaceException").eval(scope) == "msgInterfaceException"){
            // Update the maps with access modifiers and class definition
            currentScope += (itemName -> map)
            dataStructureList.update(itemType, dataStructureList(itemType).asInstanceOf[scala.collection.mutable.ListBuffer[BasicType]] += itemName)
            accessMap.update("public", accessMap("public").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (publicClassMap.head._1 -> publicClassMap.head._2))
            accessMap.update("private", accessMap("private").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (privateClassMap.head._1 -> privateClassMap.head._2))
            accessMap.update("protected", accessMap("protected").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (protectedClassMap.head._1 -> protectedClassMap.head._2))
            currentScope(itemName)
          }
          else
          {
            val output = Variable("msgInterfaceException").eval(scope)
            scope.remove("msgInterfaceException")
            output
          }
        }
      }

      this match {

        case Params(params*) =>
          // Return the list of formal parameters for Methods and Constructors
          params

        case Value(i) => i

        case Variable(name) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          if(scope.contains(name))
            scope(name)
          else
            Value(name).eval(scope)


        case Check(set, item) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val s: scala.collection.mutable.Set[BasicType] = set.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          s.contains(item.eval(scope))


        case Assign(name, item) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return
          scope += (name -> item.eval(scope))


        case Insert(set, item) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val key = set.eval(scope)

          val keyName = scope.find(_._2 == key).map(_._1) match {
            case Some(m) => m.asInstanceOf[String]
            case None => {
              // Using our own language's Try-Catch instead of Scala's try-catch
              ExceptionClassDef("InvalidTypeException", Field("InvalidTypeReason")).eval(scope)
              ThrowException(ExceptionClassDef("InvalidTypeException"), "Invalid type").eval(scope)
              CatchException(ExceptionClassDef("InvalidTypeException")).eval(scope)
            }
          }

          // Insert an item into set only when no exception has caught
          if(Variable("msgInvalidTypeException").eval(scope) != "msgInvalidTypeException")
          {
            val output = Variable("msgInvalidTypeException").eval(scope)
            scope.remove("msgInvalidTypeException")
            output
          }
          else{
            scope.update(keyName, set.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]] += item.eval(scope))
            scope(keyName)
          }


        case Delete(set, item) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val key = set.eval(scope)

          val keyName = scope.find(_._2 == key).map(_._1) match {
            case Some(m) => m.asInstanceOf[String]
            case None => {
              // Using our own language's Try-Catch instead of Scala's try-catch
              ExceptionClassDef("InvalidTypeException", Field("InvalidTypeReason")).eval(scope)
              ThrowException(ExceptionClassDef("InvalidTypeException"), "Invalid type").eval(scope)
              CatchException(ExceptionClassDef("InvalidTypeException")).eval(scope)
            }
          }

          // Delete an item from set only if no exception has been caught
          if(Variable("msgInvalidTypeException").eval(scope) == "msgInvalidTypeException")
          {
            val output = Variable("msgInvalidTypeException").eval(scope)
            scope.remove("msgInvalidTypeException")
            output
          }
          else{
            scope.update(keyName, set.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]] += item.eval(scope))
            scope(keyName)
          }


        case Union(set1, set2) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val result: scala.collection.mutable.Set[BasicType] = s1.union(s2)
          result


        case Intersect(set1, set2) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val result: scala.collection.mutable.Set[BasicType] = s1.intersect(s2)
          result


        case Diff(set1, set2) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val list_concat: scala.collection.mutable.Set[BasicType] = s1.union(s2)
          val list_intersect: scala.collection.mutable.Set[BasicType] = s1.intersect(s2)
          val result = list_concat.diff(list_intersect)
          result


        case Cross(set1, set2) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val s1: scala.collection.mutable.Set[BasicType] = set1.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val s2: scala.collection.mutable.Set[BasicType] = set2.eval(scope).asInstanceOf[scala.collection.mutable.Set[BasicType]]
          val result: scala.collection.mutable.Set[BasicType] = s1.flatMap(a => s2.map(b => (a, b)))
          result


        case SetMacro(macroName, op) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          macroMap += (macroName -> op)


        case GetMacro(macroName) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          macroMap(macroName)


        case Scope(name, op*) =>
          // Check if exception has been thrown or not
          if(instance.isExceptionThrown)
            return

          val key = if(name.equals("")){
            "anon"
          }
          else
            name

          // Get the current scope
          val sc = scope.get(key)

          // If a scope exists then return that scope otherwise create a new one
          val currentScope = sc match {
            case Some(s) => s.asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]

            case None =>
              val temp: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()
              scope += (key -> temp)
              temp
          }

          // Perform operations in the current scope
          val stack = Stack[BasicType]()
          op.foreach(i => {
            val output = i.eval(currentScope)
            stack.push(output)
          })

          // If in a scope, there are multiple statements being executed but when an exception is thrown,
          // no statement should be executed after that and only the catching of exception should be executed
          // The stack identifies the last executed statement and returns the result
          while(stack.top.getClass.toString == "class scala.runtime.BoxedUnit") {
            stack.pop()
          }

          stack.pop()

        case Field(name, expr*) =>
          val fieldMap = scope("field").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]

          // Creating a field map in which all the values will be added
          // If a field comes only with name, we assign null value to it. Otherwise the 1st value of expr is assigned to it
          if(expr.isEmpty)
            fieldMap += (name -> null)
            scala.collection.mutable.Map(name -> null)
          else
            fieldMap += (name -> expr.head.eval(scope))
            scala.collection.mutable.Map(name -> expr.head.eval(scope))

        case Constructor(expr) =>
          if(scope.contains("constructor")) {
            val constructorMap = scope("constructor").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
            // Adding the evaluated expressions to the constructor map
            expr.eval(constructorMap)
          }
          else {
            ExceptionClassDef("ConstructorException", Field("ConstructorReason")).eval(scope)
            ThrowException(ExceptionClassDef("ConstructorException"), "Cannot create constructor of abstract class").eval(scope)
            val output = CatchException(ExceptionClassDef("ConstructorException")).eval(scope)
            scope.remove("msgConstructorException")
            output
          }


        case CreateMethod(methodName, params, expr*) =>
          // Create a method map scope in which we add the formal parameters and the method definition statements
          val methodMap = scope("method").asInstanceOf[scala.collection.mutable.Map[BasicType, ListBuffer[SetExp]]]
          val list = new ListBuffer[SetExp]
          list.addOne(params)

          expr.foreach(i => list.addOne(i))
          methodMap += (methodName -> list)
          scala.collection.mutable.Map(methodName -> list)


        case InvokeMethod(method, formalparam, actualparam) =>
          // Get the list of formal and actual parameters
          val fp: immutable.ArraySeq[String] = formalparam.eval(scope).asInstanceOf[immutable.ArraySeq[String]]
          val ap: immutable.ArraySeq[SetExp] = actualparam.asInstanceOf[immutable.ArraySeq[SetExp]]

          // Check if the length of formal and actual parameters is same
          // If they are same, perform the next steps. Otherwise throw an exception
          if(fp.length.equals(ap.length)) {

            // Zip formal parameters with actual parameters
            val list = fp.zip(ap)
            list.foreach(i => {
              scope += (i._1 -> i._2.eval(scope))
            })

            // Evaluate all the statements inside the function definition except the last one
            method.zipWithIndex.foreach(i => {
              if(i._2 != method.length-1) {
                i._1.eval(scope)
              }
            })

            // Evaluate the last term of the function definition and return the result
            method.last.eval(scope)
          }
          else {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("InsufficientException", Field("InsufficientParameterReason")).eval(scope)
            ThrowException(ExceptionClassDef("InsufficientException"), "Insufficient parameters").eval(scope)
            val output = CatchException(ExceptionClassDef("InsufficientException")).eval(scope)
            scope.remove("msgInsufficientException")
            output
          }


        case NewObject(className, expr, params, values, parentObj*) =>
          val parameters = params.eval(scope).asInstanceOf[immutable.ArraySeq[String]]

          // Check if formal parameters and actual parameters for constructors are equal
          if(parameters.length != values.length) {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("ParameterException", Field("InsufficientParameterReason")).eval(scope)
            ThrowException(ExceptionClassDef("ParameterException"), "Parameter list doesn't have equal number of initializing values").eval(scope)
            val output = CatchException(ExceptionClassDef("ParameterException")).eval(scope)
            scope.remove("msgParameterException")
            output
          }
          else{
            // Create a list of values being passed to the constructor
            val valueList: ListBuffer[BasicType] = ListBuffer()
            values.foreach(i => {
              valueList.addOne(i.eval(scope))
            })

            // Zip the formal parameters with actual parameters
            val result = parameters zip valueList

            val initializationMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()

            // Initialization map will have the new values for the fields for a particular object
            result.foreach(i => {
              initializationMap += (i._1 -> i._2)
            })

            // Check if already an object exists for the class
            if(objectMap.contains(className)){
              // Check if any value for the object of outer class provided or not
              // If creating for outer class, parentObj would not be provided. Otherwise name of outer class object should be provided
              if(parentObj.isEmpty) {
                // Get the list of objects for the current class
                val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
                list += expr.eval(scope)

                // Update the objectMap to include the current object to the class list
                objectMap += (className -> list)
                // Update the attrMap to include the current object and the list of elements it can access
                attrMap += (expr.eval(scope) -> scope(className).asInstanceOf[scala.collection.mutable.Map[String, Any]].clone())

                // If we have an inner class, the object should not access that. Hence, we remove the innerInterface entry from the map
                val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[scala.collection.mutable.Map[String, Any]]
                objectAttr -= "innerClass"
                attrMap += (expr.eval(scope) -> objectAttr)
              }
              else {
                // Parent object was provided for the current object
                // Check if the object's class is an inner class or not
                if(nestedClassMap.exists(_._2.asInstanceOf[String] == className)){
                  val list: ListBuffer[Any] = objectMap(className).asInstanceOf[ListBuffer[Any]]
                  list += expr.eval(scope)
                  objectMap += (className -> list)

                  // Get the name of the outer class
                  val outerClassName = nestedClassMap.find(_._2.asInstanceOf[String] == className).map(_._1) match {
                    case Some(m) => m
                    case None => {
                      // Using our own language's Try-Catch instead of Scala's try-catch
                      ExceptionClassDef("InsufficientException", Field("InsufficientParameterReason")).eval(scope)
                      ThrowException(ExceptionClassDef("InsufficientException"), "Insufficient parameters").eval(scope)
                      CatchException(ExceptionClassDef("InsufficientException")).eval(scope)
                    }
                  }

                  if(Variable("msgInsufficientException").eval(scope) == "msgInsufficientException"){
                    // Get the list of objects created for the outer class
                    val outerClassObjects = objectMap(outerClassName).asInstanceOf[ListBuffer[Any]]

                    // Check if the parent object exists or not
                    if(outerClassObjects.contains(parentObj.head)) {
                      val outerClassAttr = scope(outerClassName).asInstanceOf[scala.collection.mutable.Map[String, Any]]
                      // Associate the members of the inner class with the current object
                      attrMap += (expr.eval(scope) -> outerClassAttr("innerClass").asInstanceOf[mutable.Map[String, Any]](className))
                    }
                    else {
                      // Throw an exception because outer class object doesn't exist
                      // Using our own language's Try-Catch instead of Scala's try-catch
                      ExceptionClassDef("NoOuterObjectException", Field("NoOuterObjectReason")).eval(scope)
                      ThrowException(ExceptionClassDef("NoOuterObjectException"), "Outer class object doesn't exist").eval(scope)
                      CatchException(ExceptionClassDef("NoOuterObjectException")).eval(scope)
                    }
                  }
                }
                else {
                  // Throw an exception because the current object's class is not an inner class
                  // Using our own language's Try-Catch instead of Scala's try-catch
                  ExceptionClassDef("NoObjectNeededException", Field("NoObjectNeededReason")).eval(scope)
                  ThrowException(ExceptionClassDef("NoObjectNeededException"), "Outer class object not needed").eval(scope)
                  CatchException(ExceptionClassDef("NoObjectNeededException")).eval(scope)
                }
              }

              // Once the object has been created, the constructor for the object should be called that will initialize the fields
              // Once the Constructor statements have been executed, remove the fields from constructor map and add to the fields map
              val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[scala.collection.mutable.Map[String, Any]].clone()
              val objectConstructor = objectAttr("constructor").asInstanceOf[scala.collection.mutable.Map[String, Any]].clone()
              val objectField = objectAttr("field").asInstanceOf[scala.collection.mutable.Map[String, Any]].clone()
              val result = objectConstructor.++(objectField)

              initializationMap.foreach(i => {
                if(!result.contains(i._1))
                  // Using our own language's Try-Catch instead of Scala's try-catch
                  ExceptionClassDef("NoFieldException", Field("NoFieldReason")).eval(scope)
                  ThrowException(ExceptionClassDef("NoFieldException"), "Class doesn't have field").eval(scope)
                  CatchException(ExceptionClassDef("NoFieldException")).eval(scope)
              })

              // Check if any of the exceptions defined in the code block have been caught
              if(Variable("msgNoOuterObjectException").eval(scope) == "msgNoOuterObjectException" &&
                Variable("msgNoObjectNeededException").eval(scope) == "msgNoObjectNeededException" &&
                Variable("msgNoFieldException").eval(scope) == "msgNoFieldException")
              {
                // Update the attrMap to hold the new values for fields as they have been initialized after calling the constructor
                val finalMap = result.++(initializationMap)
                attrMap.update(expr.eval(scope), objectAttr += ("field" -> finalMap))
                attrMap.update(expr.eval(scope), objectAttr -= "constructor")
              }
              else if(Variable("msgNoOuterObjectException").eval(scope) != "msgNoOuterObjectException")
              {
                output = Variable("msgNoOuterObjectException").eval(scope)
                scope.remove("msgNoOuterObjectException")
              }
              else if(Variable("msgNoObjectNeededException").eval(scope) != "msgNoObjectNeededException")
              {
                output = Variable("msgNoObjectNeededException").eval(scope)
                scope.remove("msgNoObjectNeededException")
              }
              else if(Variable("msgNoFieldException").eval(scope) != "msgNoFieldException")
              {
                output = Variable("msgNoFieldException").eval(scope)
                scope.remove("msgNoFieldException")
              }

              output
            }
            else {
              // Creating the object of a class for the 1st time
              // Check if any value for the object of outer class provided or not
              // If creating for outer class, parentObj would not be provided. Otherwise name of outer class object should be provided
              if(parentObj.isEmpty) {
                // Create the list of objects for the current class
                val list: ListBuffer[Any] = ListBuffer()
                list += expr.eval(scope)
                // Update the objectMap to include the current object to the class list
                objectMap += (className -> list)
                // Update the attrMap to include the current object and the list of elements it can access
                attrMap += (expr.eval(scope) -> scope(className).asInstanceOf[scala.collection.mutable.Map[String, Any]].clone())

                // If we have an inner class, the object should not access that. Hence, we remove the innerInterface entry from the map
                val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[scala.collection.mutable.Map[String, Any]]
                objectAttr -= "innerClass"
                attrMap += (expr.eval(scope) -> objectAttr)
              }
              else {
                // Parent object was provided for the current object
                // Check if the object's class is an inner class or not
                if(nestedClassMap.exists(_._2.asInstanceOf[String] == className)){

                  // Get the name of the outer class
                  val outerClassName = nestedClassMap.find(_._2.asInstanceOf[String] == className).map(_._1) match {
                    case Some(m) => m
                    case None => throw new Error("Outer class not found")
                  }

                  // Get the list of objects created for the outer class
                  val outerClassObjects = objectMap(outerClassName).asInstanceOf[ListBuffer[Any]]

                  // Check if the parent object exists or not
                  if(outerClassObjects.contains(parentObj.head)) {
                    val list: ListBuffer[Any] = ListBuffer()
                    list += expr.eval(scope)
                    objectMap += (className -> list)
                    val outerClassAttr = scope(outerClassName).asInstanceOf[scala.collection.mutable.Map[String, Any]]
                    // Associate the members of the inner class with the current object
                    attrMap += (expr.eval(scope) -> outerClassAttr("innerClass").asInstanceOf[mutable.Map[String, Any]](className))
                  }
                  else {
                    // Throw an exception because outer class object doesn't exist
                    // Using our own language's Try-Catch instead of Scala's try-catch
                    ExceptionClassDef("ParentExistException", Field("ParentExistReason")).eval(scope)
                    ThrowException(ExceptionClassDef("ParentExistException"), "Parent object doesn't exist").eval(scope)
                    CatchException(ExceptionClassDef("ParentExistException")).eval(scope)
                  }
                }
                else {
                  // Throw an exception because the current object's class is not an inner class
                  // Using our own language's Try-Catch instead of Scala's try-catch
                  ExceptionClassDef("NoClassObjectException", Field("NoClassObjectReason")).eval(scope)
                  ThrowException(ExceptionClassDef("NoClassObjectException"), "Class is not an inner class. Parent object not needed").eval(scope)
                  CatchException(ExceptionClassDef("NoClassObjectException")).eval(scope)
                }
              }

              // Once the object has been created, the constructor for the object should be called that will initialize the fields
              // Once the Constructor statements have been executed, remove the fields from constructor map and add to the fields map
              val objectAttr = attrMap(expr.eval(scope)).asInstanceOf[scala.collection.mutable.Map[String, Any]].clone()
              val objectConstructor = objectAttr("constructor").asInstanceOf[scala.collection.mutable.Map[String, Any]].clone()
              val objectField = objectAttr("field").asInstanceOf[scala.collection.mutable.Map[String, Any]].clone()
              val result = objectConstructor.++(objectField)

              initializationMap.foreach(i => {
                if(!result.contains(i._1))
                  // Using our own language's Try-Catch instead of Scala's try-catch
                  ExceptionClassDef("NoFieldException", Field("NoFieldReason")).eval(scope)
                  ThrowException(ExceptionClassDef("NoFieldException"), "Class doesn't have field").eval(scope)
                  CatchException(ExceptionClassDef("NoFieldException")).eval(scope)
              })

              // Check if any of the exceptions defined in the code block have been caught
              if(Variable("msgParentExistException").eval(scope) == "msgParentExistException" &&
                Variable("msgNoClassObjectException").eval(scope) == "msgNoClassObjectException" &&
                Variable("msgNoFieldException").eval(scope) == "msgNoFieldException")
              {
                // Update the attrMap to hold the new values for fields as they have been initialized after calling the constructor
                val finalMap = result.++(initializationMap)
                attrMap.update(expr.eval(scope), objectAttr += ("field" -> finalMap))
                attrMap.update(expr.eval(scope), objectAttr -= "constructor")
              }
              // Check which exception has been caught and return the error message
              else if(Variable("msgParentExistException").eval(scope) != "msgParentExistException")
              {
                output = Variable("msgParentExistException").eval(scope)
                scope.remove("msgParentExistException")
              }
              else if(Variable("msgNoClassObjectException").eval(scope) != "msgNoClassObjectException")
              {
                output = Variable("msgNoClassObjectException").eval(scope)
                scope.remove("msgNoClassObjectException")
              }
              else if(Variable("msgNoFieldException").eval(scope) != "msgNoFieldException")
              {
                output = Variable("msgNoFieldException").eval(scope)
                scope.remove("msgNoFieldException")
              }

              output
            }
          }

        case InvokeObject(className, objectName, attrName, actualParams*) =>
          // Check if we have created the class for which we want to use an object
          if(!objectMap.contains(className.eval(scope)))
          {
            // Using our own language's Try-Catch instead of Scala's try-catch
            ExceptionClassDef("NoClassObjectException", Field("NoClassObjectReason")).eval(scope)
            ThrowException(ExceptionClassDef("NoClassObjectException"), "Class doesn't have this object").eval(scope)
            CatchException(ExceptionClassDef("NoClassObjectException")).eval(scope)
          }
          else {
            // Get the list of objects created for the current class
            val list: ListBuffer[BasicType] = objectMap(className.eval(scope)).asInstanceOf[ListBuffer[BasicType]]

            // Check if we have created the object for which we want to invoke a method or get a field
            if(!list.contains(objectName.eval(scope)))
            {
              // Using our own language's Try-Catch instead of Scala's try-catch
              ExceptionClassDef("NoObjectException", Field("NoObjectReason")).eval(scope)
              ThrowException(ExceptionClassDef("NoObjectException"), "There is no such object").eval(scope)
              CatchException(ExceptionClassDef("NoObjectException")).eval(scope)
            }
            else {
              // Get the elements that can be accessed by the object from attrMap
              val map: scala.collection.mutable.Map[BasicType, BasicType] = attrMap(objectName.eval(scope)).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]

              // Check if any actual parameters provided for the method
              // If they are not provided, we want to access a variable of the class
              if(actualParams.isEmpty){
                val fieldMap = map("field").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
                fieldMap(attrName.eval(scope))
              }
              else{
                // Get the list of methods the object can access
                val methodMap = map("method").asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
                val method = methodMap(attrName.eval(scope)).asInstanceOf[ListBuffer[SetExp]]
                // Fetch the formal parameters
                val formalParams = method.head
                val tempMap: scala.collection.mutable.Map[Any, Any] = scala.collection.mutable.Map()
                // Invoke the method by passing the method definition, formal parameters and actual parameters
                InvokeMethod(method.drop(1), formalParams, actualParams).eval(tempMap)
              }
            }
          }

        case Public(expr) =>
          // From eval(), we get the map at 0th index to store members with public access
          val publicMap = access(0)
          val result = expr.eval(scope).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
          publicMap.update(publicMap.head._1, publicMap(publicMap.head._1).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case Private(expr) =>
          // From eval(), we get the map at 1st index to store members with private access
          val privateMap = access(1)
          val result = expr.eval(scope).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
          privateMap.update(privateMap.head._1, privateMap(privateMap.head._1).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case Protected(expr) =>
          // From eval(), we get the map at 2nd index to store members with protected access
          val protectedMap = access(2)
          val result = expr.eval(scope).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]]
          protectedMap.update(protectedMap.head._1, protectedMap(protectedMap.head._1).asInstanceOf[scala.collection.mutable.Map[BasicType, BasicType]] += (result.head._1 -> result.head._2))


        case ClassDef(className, expr*) =>
          // Create a class using helper method
          createRefHelper(className, "class", scope, expr*)

        case AbstractClassDef(className, expr*) =>
          // Create an abstract class using helper method
          createRefHelper(className, "abstract_class", scope, expr*)

        case Interface(interfaceName, expr*) =>
          // Create an interface using helper method
          createRefHelper(interfaceName, "interface", scope, expr*)

        case InnerClass(outerClass, innerClass) =>
          // Create an inner class using helper method
          createInnerRefHelper(innerClass, outerClass, "innerClass", scope)

        case InnerInterface(outerInterface, innerInterface) =>
          // Create an inner interface using helper method
          createInnerRefHelper(innerInterface, outerInterface, "innerInterface", scope)

        case If(condition, thenClause, elseClause) =>
          condition.eval(scope) match {
            case true => thenClause.eval(scope)   // If condition evaluated to true
            case _ => elseClause.eval(scope)      // If condition evaluated to false
          }

        case Then(clauses*) =>
          // Evaluate all the statements present inside 'then' clause of If-condition
          clauses.foreach(i => {
            i.eval(scope)
          })

        case Else(clauses*) =>
          // Evaluate all the statements present inside 'else' clause of If-condition
          clauses.foreach(i => {
            i.eval(scope)
          })

        case ExceptionClassDef(className, excpVar*) =>
          // If an exception class already exists, return the class
          if (scope.contains(className))
            scope(className)
          else {
            // Each exception class should have 1 field
            if(excpVar.size == 0)
              throw new Error("Exception class should have a field")

            // Create a map to store the values of exception class
            val map: scala.collection.mutable.Map[BasicType, BasicType] = scala.collection.mutable.Map()
            val fieldMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
            map += ("field" -> fieldMap)

            // Create a field for the exception class
            excpVar.head.eval(map)
            scope += (className -> map)
            scope(className)
          }

        case ThrowException(className, excpMsg) =>
          // Get the field map from the exception class
          val fieldSet = className.eval(scope).asInstanceOf[scala.collection.mutable.Map[String, BasicType]].clone()("field")
          // Get the name of the exception class variable
          val excpVar = fieldSet.asInstanceOf[scala.collection.mutable.Map[String, BasicType]].keySet.head

          // Get the name of the exception class
          val myClass = scope.find(_._2 == className.eval(scope)).map(_._1) match {
            case Some(m) => m.asInstanceOf[String]
            case _ => throw new Error("Class Not found")
          }

          // Update the exception class map with the exception message assigne to the variable
          scope.update(myClass, mutable.Map("field" -> mutable.Map(excpVar -> excpMsg)))

          // Another method to update the value of 'val' by creating the variable as a field of a class
          // Setting isExceptionThrown = true to bypass statements between ThrowException and CatchException
          val f = instance.getClass.getDeclaredField("isExceptionThrown")
          f.setAccessible(true)
          f.setBoolean(instance, true)
          scope(myClass)

        case CatchException(className) =>
          // Executed only if an exception is thrown
          if(!instance.isExceptionThrown)
            return

          // Get the field map from the exception class
          val fieldSet = className.eval(scope).asInstanceOf[scala.collection.mutable.Map[String, BasicType]].clone()("field")

          val myClass = scope.find(_._2 == className.eval(scope)).map(_._1) match {
            case Some(m) => m.asInstanceOf[String]
            case _ => throw new Error("Class Not found")
          }

          // Get the name of the field
          val key = fieldSet.asInstanceOf[scala.collection.mutable.Map[String, BasicType]].keySet.head

          // Get the error message associated with the field
          val msg = fieldSet.asInstanceOf[scala.collection.mutable.Map[String, BasicType]](key)

          // Once the exception has been caught, change the status of isExceptionThrown to false
          val f = instance.getClass.getDeclaredField("isExceptionThrown")
          f.setAccessible(true)
          f.setBoolean(instance, false)

          // Assign the message to a variable
          Assign("msg" + myClass, Value(msg)).eval(scope)

          // Return the value of error message
          Variable("msg"+myClass).eval(scope)
      }

  @main def runArithExp: Unit =
    import SetExp.*