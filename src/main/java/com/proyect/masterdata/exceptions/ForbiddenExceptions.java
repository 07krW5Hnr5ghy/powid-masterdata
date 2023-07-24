package com.proyect.masterdata.exceptions;

import lombok.Data;

@Data
public class ForbiddenExceptions extends RuntimeException{
    public ForbiddenExceptions(String message) {
        super(message);
    }
}
