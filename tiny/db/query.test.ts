export class MockUniqueViolationConstraintError extends Error {
  code = '23505'

  constructor(constraintName: string) {
    super(`violates unique constraint "${constraintName}"`)
  }
}
