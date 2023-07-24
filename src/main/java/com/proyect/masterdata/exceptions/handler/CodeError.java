package com.proyect.masterdata.exceptions.handler;

public enum CodeError {
    BADREQUEST(400, "Bad Request"),
    UNAUTHORIZED(401,"Unauthorized"),
    FORBIDDEN(403,"ForbiddenForbidden"),
    NOTFOUND(404,"Not Found"),
    CONFLICT(409,"Conflict"),
    INTERNALSERVERERRO(500,"Internal Server Error");

    private int code;
    private String message;

    CodeError(int code, String message) {
        this.code = code;
        this.message = message;
    }

    public int getCode() {
        return code;
    }

    public String getMessage() {
        return message;
    }
}
