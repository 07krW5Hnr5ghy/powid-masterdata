package com.proyect.masterdata.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestProvince {
    private Long code;
    private String name;
    private boolean status;
    private String user;
    private Long codeDepartment;
}
