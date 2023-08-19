package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Province;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ProvinceRepositoryCustom {
    Page<Province> searchForDepartment(String name,
                                       String user,
                                       Long idDepartment,
                                       String nameDepartment,
                                       String sort,
                                       String sortColumn,
                                       Integer pageNumber,
                                       Integer pageSize,
                                       Boolean status);
}
