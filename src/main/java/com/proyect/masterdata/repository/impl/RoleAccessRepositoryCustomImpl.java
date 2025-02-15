package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.RoleAccess;
import com.proyect.masterdata.repository.RoleAccessRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class RoleAccessRepositoryCustomImpl implements RoleAccessRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<RoleAccess> searchForRoleAccess(
            UUID roleId, 
            UUID accessId, 
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize, 
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<RoleAccess> criteriaQuery = criteriaBuilder.createQuery(RoleAccess.class);

        Root<RoleAccess> itemRoot = criteriaQuery.from(RoleAccess.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(roleId, accessId, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> roleAccessList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                roleAccessList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                roleAccessList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(roleAccessList);

        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<RoleAccess> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(roleId, accessId, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(UUID roleId, UUID accessId, Boolean status,
                                      CriteriaBuilder criteriaBuilder, Root<RoleAccess> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (roleId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("roleId"), roleId)));
        }

        if (accessId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("accessId"), accessId)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;

    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<RoleAccess> itemRoot) {

        List<Order> roleAccessList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("roleId")) {
            roleAccessList.add(criteriaBuilder.asc(itemRoot.get("roleId")));
        }

        if (sortColumn.equalsIgnoreCase("accessId")) {
            roleAccessList.add(criteriaBuilder.asc(itemRoot.get("accessId")));
        }

        return roleAccessList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<RoleAccess> itemRoot) {

        List<Order> roleAccessList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("roleId")) {
            roleAccessList.add(criteriaBuilder.desc(itemRoot.get("roleId")));
        }

        if (sortColumn.equalsIgnoreCase("accessId")) {
            roleAccessList.add(criteriaBuilder.desc(itemRoot.get("accessId")));
        }

        return roleAccessList;
    }

    private Long getOrderCount(UUID roleId, UUID accessId, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<RoleAccess> itemRoot = criteriaQuery.from(RoleAccess.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(roleId, accessId, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
