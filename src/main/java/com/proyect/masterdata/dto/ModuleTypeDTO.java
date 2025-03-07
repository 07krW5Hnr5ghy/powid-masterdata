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
public class ModuleTypeDTO {
    private String user;
    private Boolean status;
    private UUID id;
    private String userType;
    private String module;
}
