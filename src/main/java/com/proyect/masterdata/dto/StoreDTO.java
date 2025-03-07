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
public class StoreDTO {
    private UUID id;
    private String name;
    private String url;
    private String client;
    private String storeType;
    private String user;
    private Boolean status;
}
