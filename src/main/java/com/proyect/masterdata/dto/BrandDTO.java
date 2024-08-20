package com.proyect.masterdata.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class BrandDTO {
    private String name;
    private String client;
    private Date registrationDate;
    private Date updateDate;
    private String tokenUser;
}
