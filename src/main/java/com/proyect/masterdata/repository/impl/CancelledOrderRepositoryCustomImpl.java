package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.CancelledOrder;
import com.proyect.masterdata.repository.CancelledOrderRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class CancelledOrderRepositoryCustomImpl implements CancelledOrderRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<CancelledOrder> searchForCancelledOrder(Long orderId, Long clientId, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<CancelledOrder> criteriaQuery = criteriaBuilder.createQuery(CancelledOrder.class);
        Root<CancelledOrder> itemRoot = criteriaQuery.from(CancelledOrder.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(orderId, clientId, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> cancelledOrderList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                cancelledOrderList = listAsc(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                cancelledOrderList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(cancelledOrderList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<CancelledOrder> orderTypeQuery = entityManager.createQuery(criteriaQuery);
        orderTypeQuery.setFirstResult(pageNumber * pageSize);
        orderTypeQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(orderId, clientId);
        return new PageImpl<>(orderTypeQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            Long orderId,
            Long clientId,
            CriteriaBuilder criteriaBuilder,
            Root<CancelledOrder> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (orderId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderId"), orderId)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        return conditions;
    }

    List<Order> listAsc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CancelledOrder> itemRoot) {

        List<Order> cancelledOrderList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderId")) {
            cancelledOrderList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            cancelledOrderList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        return cancelledOrderList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CancelledOrder> itemRoot) {

        List<Order> cancelledOrderList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("orderId")) {
            cancelledOrderList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }

        if (sortColumn.equalsIgnoreCase("clientId")) {
            cancelledOrderList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        return cancelledOrderList;
    }

    private long getOrderCount(Long orderId, Long clientId) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<CancelledOrder> itemRoot = criteriaQuery.from(CancelledOrder.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(orderId, clientId, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
