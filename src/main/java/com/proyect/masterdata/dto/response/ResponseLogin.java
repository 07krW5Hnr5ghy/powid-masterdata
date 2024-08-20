package com.proyect.masterdata.dto.response;

import com.proyect.masterdata.domain.User;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ResponseLogin {
    private User user;
    private String jwt;
    private int code;
    private String message;
}
