package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ModuleDTO {
    Long code;
    String moduleName;
    Double modulePrice;
    int moduleStatus;
    Boolean status;
}
