pub type Program {
  Program(
    consts: List(JEFValue),
    functions: List(FunctionMetaData),
    instructions: List(Instruction),
  )
}

pub type JEFValue {
  Int(Int)
  Float(Float)
  String(String)
  Bool(Bool)
}

pub type FunctionMetaData {
  FunctionMetaData(address: Int, arity: Int, locals: Int)
}

pub type Instruction {
  Instruction(opcode: String, args: List(JEFValue))
}
