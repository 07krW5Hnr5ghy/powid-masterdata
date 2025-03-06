package com.proyect.masterdata.repository.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Repository;

import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.repository.SubscriptionRepositoryCustom;

import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

@Repository
public class SubscriptionRepositoryCustomImpl implements SubscriptionRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Subscription> searchForSubscription(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Subscription> criteriaQuery = criteriaBuilder.createQuery(Subscription.class);
        Root<Subscription> itemRoot = criteriaQuery.from(Subscription.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, user, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sortColumn) && !StringUtils.isBlank(sort)) {
            List<Order> subscriptionList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                subscriptionList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                subscriptionList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(subscriptionList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Subscription> ordeTypedQuery = entityManager.createQuery(criteriaQuery);
        ordeTypedQuery.setFirstResult(pageNumber * pageSize);
        ordeTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(name, user, status);
        return new PageImpl<>(ordeTypedQuery.getResultList(), pageable, count);

    }

    public List<Predicate> predicateConditions(
            String name,
            String user,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<Subscription> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (user != null) {
            conditions.add(
                    criteriaBuilder.and(criteriaBuilder.equal(
                            criteriaBuilder.upper(itemRoot.get("user")), user.toUpperCase())));
        }

        if(Boolean.TRUE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if(Boolean.FALSE.equals(status)) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Subscription> itemRoot) {

        List<Order> subscriptionList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            subscriptionList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("USER")) {
            subscriptionList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }

        return subscriptionList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Subscription> itemRoot) {
        List<Order> subscriptionList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            subscriptionList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("USER")) {
            subscriptionList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }

        return subscriptionList;
    }

    private Long getOrderCount(String name, String user, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Subscription> itemRoot = criteriaQuery.from(Subscription.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, user, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}