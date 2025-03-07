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
public class ModuleDTO {
    private UUID id;
    private String moduleName;
    private Double modulePrice;
    private String user;
    private Boolean status;
}
