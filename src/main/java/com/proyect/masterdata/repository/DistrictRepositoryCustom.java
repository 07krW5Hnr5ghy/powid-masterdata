package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.District;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface DistrictRepositoryCustom {
    Page<District> searchForDistrict(String name,
                                       String user,
                                       UUID idProvince,
                                       String nameProvince,
                                       String sort,
                                       String sortColumn,
                                       Integer pageNumber,
                                       Integer pageSize,
                                       Boolean status);
}
