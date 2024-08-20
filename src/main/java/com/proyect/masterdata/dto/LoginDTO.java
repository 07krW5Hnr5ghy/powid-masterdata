package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class LoginDTO {
    private String username;
    private List<String> roles;
    private String jwt;
    private Integer code;
    private String message;
}
