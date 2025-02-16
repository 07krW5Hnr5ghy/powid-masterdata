package com.proyect.masterdata.dto.request;

import com.proyect.masterdata.domain.Province;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestDistrictSave {
    private String name;
    private UUID codeProvince;
    private Province province;
}
