package com.proyect.masterdata.exceptions.handler;

import com.proyect.masterdata.exceptions.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

@ControllerAdvice
public class RestResponseEntityExceptionHandler {
    @ExceptionHandler(value = BadRequestExceptions.class)
    public ResponseEntity<ErrorHandler> handlerBadRequestExceptions(RuntimeException ex){
        ErrorHandler errorHandler = ErrorHandler.builder()
                .code(CodeError.BADREQUEST.getCode())
                .errorCode(CodeError.BADREQUEST.getMessage())
                .message(ex.getMessage())
                .build();
        return new ResponseEntity<>(errorHandler, HttpStatus.BAD_REQUEST);
    }

    @ExceptionHandler(value = UnauthorizedExceptions.class)
    public ResponseEntity<ErrorHandler> handlerUnauthorizedExceptions(RuntimeException ex){
        ErrorHandler errorHandler = ErrorHandler.builder()
                .code(CodeError.UNAUTHORIZED.getCode())
                .errorCode(CodeError.UNAUTHORIZED.getMessage())
                .message(ex.getMessage())
                .build();
        return new ResponseEntity<>(errorHandler, HttpStatus.UNAUTHORIZED);
    }

    @ExceptionHandler(value = ForbiddenExceptions.class)
    public ResponseEntity<ErrorHandler> handlerForbiddenExceptions(RuntimeException ex){
        ErrorHandler errorHandler = ErrorHandler.builder()
                .code(CodeError.FORBIDDEN.getCode())
                .errorCode(CodeError.FORBIDDEN.getMessage())
                .message(ex.getMessage())
                .build();
        return new ResponseEntity<>(errorHandler, HttpStatus.FORBIDDEN);
    }

    @ExceptionHandler(value = NotFoundExceptions.class)
    public ResponseEntity<ErrorHandler> handlerNotFoundExceptions(RuntimeException ex){
        ErrorHandler errorHandler = ErrorHandler.builder()
                .code(CodeError.NOTFOUND.getCode())
                .errorCode(CodeError.NOTFOUND.getMessage())
                .message(ex.getMessage())
                .build();
        return new ResponseEntity<>(errorHandler, HttpStatus.NOT_FOUND);
    }

    @ExceptionHandler(value = ConflictExceptions.class)
    public ResponseEntity<ErrorHandler> handlerConflictExceptions(RuntimeException ex){
        ErrorHandler errorHandler = ErrorHandler.builder()
                .code(CodeError.CONFLICT.getCode())
                .errorCode(CodeError.CONFLICT.getMessage())
                .message(ex.getMessage())
                .build();
        return new ResponseEntity<>(errorHandler, HttpStatus.CONFLICT);
    }

    @ExceptionHandler(value = InternalErrorExceptions.class)
    public ResponseEntity<ErrorHandler> handlerInternalErrorExceptions(RuntimeException ex){
        ErrorHandler errorHandler = ErrorHandler.builder()
                .code(CodeError.INTERNALSERVERERRO.getCode())
                .errorCode(CodeError.INTERNALSERVERERRO.getMessage())
                .message(ex.getMessage())
                .build();
        return new ResponseEntity<>(errorHandler, HttpStatus.INTERNAL_SERVER_ERROR);
    }


}
