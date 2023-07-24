package com.proyect.masterdata.exceptions;

import lombok.Data;

@Data
public class NotFoundExceptions extends RuntimeException{
    public NotFoundExceptions(String message) {
        super(message);
    }
}
