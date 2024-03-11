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

@Repository
public class UserRepositoryCustomImpl implements UserRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<User> searchForUser(
            String user,
            Long clientId,
            String dni,
            String email,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<User> criteriaQuery = criteriaBuilder.createQuery(User.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(user, clientId, dni, email, status, criteriaBuilder, itemRoot);

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
        long count = getOrderCount(user, clientId, dni, email, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            String username,
            Long clientId,
            String dni,
            String email,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (username != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("username")), username.toUpperCase())));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (dni != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("dni")), dni.toUpperCase())));
        }

        if (email != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("email")), email.toLowerCase())));
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot) {

        List<Order> userList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("USERNAME")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("username")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("dni")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("dni")));
        }

        if (sortColumn.equalsIgnoreCase("email")) {
            userList.add(criteriaBuilder.asc(itemRoot.get("email")));
        }

        return userList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<User> itemRoot) {

        List<Order> userList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("USERNAME")) {
            userList.add(criteriaBuilder.desc(itemRoot.get("username")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            userList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("dni")) {
            userList.add(criteriaBuilder.desc(itemRoot.get("dni")));
        }

        if (sortColumn.equalsIgnoreCase("email")) {
            userList.add(criteriaBuilder.desc(itemRoot.get("email")));
        }

        return userList;
    }

    private long getOrderCount(String username, Long clientId,String dni, String email, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<User> itemRoot = criteriaQuery.from(User.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(username, clientId, dni, email, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
