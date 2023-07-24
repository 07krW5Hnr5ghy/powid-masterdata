package com.proyect.masterdata.exceptions;

import lombok.Data;

@Data
public class BadRequestExceptions extends RuntimeException{
    public BadRequestExceptions(String message) {
        super(message);
    }
}
