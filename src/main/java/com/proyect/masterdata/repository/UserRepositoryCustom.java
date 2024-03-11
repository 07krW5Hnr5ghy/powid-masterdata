package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface UserRepositoryCustom {
    Page<User> searchForUser(
            String user,
            Long clientId,
            String dni,
            String email,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
