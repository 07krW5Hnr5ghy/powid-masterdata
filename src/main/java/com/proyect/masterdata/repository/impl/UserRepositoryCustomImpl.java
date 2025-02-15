package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.repository.UserRepositoryCustom;
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
public class UserRepositoryCustomImpl implements UserRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<User> searchForUser(
                                    UUID clientId,
                                    List<String> names,
                                    List<String> usernames,
                                    String sort,
                                    String sortColumn,
                                    Integer pageNumber,
                                    Integer pageSize,
                                    Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<User> criteriaQuery = criteriaBuilder.createQuery(User.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                names,
                usernames,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> userList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                userList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                userList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(userList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<User> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                names,
                usernames,
                status);

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    private List<Predicate> predicate(
            UUID clientId,
            List<String> names,
            List<String> usernames,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (!names.isEmpty()) {
            conditions.add(criteriaBuilder.and(itemRoot.get("name").in(names)));
        }

        if(!usernames.isEmpty()){
            conditions.add(criteriaBuilder.and(itemRoot.get("username").in(usernames)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<User> itemRoot) {

        List<Order> userList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("username")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("username")));
        }

        if (sortColumn.equalsIgnoreCase("name")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return userList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<User> itemRoot) {

        List<Order> warehouseList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("username")) {
            warehouseList.add(criteriaBuilder.desc(itemRoot.get("username")));
        }

        if (sortColumn.equalsIgnoreCase("name")) {
            warehouseList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            warehouseList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return warehouseList;

    }

    private Long getOrderCount(
            UUID clientId,
            List<String> names,
            List<String> usernames,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                names,
                usernames,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
