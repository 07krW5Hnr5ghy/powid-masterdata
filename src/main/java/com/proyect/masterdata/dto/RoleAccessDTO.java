package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RoleAccessDTO {
    private UUID id;
    private String user;
    private String roleName;
    private String accessName;
    private Boolean status;
}
