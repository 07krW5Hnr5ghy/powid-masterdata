package com.proyect.masterdata.exceptions;

import lombok.Data;

@Data
public class UnauthorizedExceptions extends RuntimeException{
    public UnauthorizedExceptions(String message) {
        super(message);
    }
}
