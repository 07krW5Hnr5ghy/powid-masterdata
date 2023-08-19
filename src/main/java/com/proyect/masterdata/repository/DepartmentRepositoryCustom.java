package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface DepartmentRepositoryCustom {
    Page<Department> searchForDepartment(String name,
                                         String user,
                                         String sort,
                                         String sortColumn,
                                         Integer pageNumber,
                                         Integer pageSize,
                                         Boolean status);

}
