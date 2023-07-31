package com.proyect.masterdata.utils;

import org.springframework.security.crypto.bcrypt.BCrypt;

public class Encrypt {
    public static final String encryptPassword(String password){
        return BCrypt.hashpw(password, BCrypt.gensalt());
    }

    public static final boolean verifyPassword(String originalPassword, String hashPassword){
        return BCrypt.checkpw(originalPassword, hashPassword);
    }
}