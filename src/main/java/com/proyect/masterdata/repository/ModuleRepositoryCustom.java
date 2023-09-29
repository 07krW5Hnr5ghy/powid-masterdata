package com.proyect.masterdata.repository;

import org.springframework.data.domain.Page;
import com.proyect.masterdata.domain.Module;
import org.springframework.stereotype.Repository;

@Repository
public interface ModuleRepositoryCustom {
    Page<Module> searchForModule(String name,
                                     String user,
                                     String sort,
                                     String sortColumn,
                                     Integer pageNumber,
                                     Integer pageSize,
                                     Boolean status);
}
