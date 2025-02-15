package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Province;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.UUID;

@Repository
public interface ProvinceRepositoryCustom {
    Page<Province> searchForProvince(String name,
                                     String user,
                                     UUID idDepartment,
                                     String nameDepartment,
                                     String sort,
                                     String sortColumn,
                                     Integer pageNumber,
                                     Integer pageSize,
                                     Boolean status);
}
