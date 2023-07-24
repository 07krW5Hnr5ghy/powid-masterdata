package com.proyect.masterdata.exceptions;

import lombok.Data;

@Data
public class InternalErrorExceptions extends RuntimeException{
    public InternalErrorExceptions(String message) {
        super(message);
    }
}
