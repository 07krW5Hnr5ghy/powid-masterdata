package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.User;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface UserRepositoryCustom {
    Page<User> searchForUser(
            UUID clientId,
            List<String> names,
            List<String> usernames,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
