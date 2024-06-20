package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.OrderReturn;
import com.proyect.masterdata.repository.OrderReturnRepositoryCustom;
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
import java.util.Date;
import java.util.List;

@Repository
public class OrderReturnRepositoryCustomImpl implements OrderReturnRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<OrderReturn> searchForOrderReturn(Long orderId, Long clientId, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderReturn> criteriaQuery = criteriaBuilder.createQuery(OrderReturn.class);
        Root<OrderReturn> itemRoot = criteriaQuery.from(OrderReturn.class);
        
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(
                orderId,
                clientId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> orderReturnList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                orderReturnList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                orderReturnList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderReturnList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<OrderReturn> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(
                orderId,
                clientId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    
    List<Predicate> predicateConditions(
            Long orderId,
            Long clientId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturn> itemRoot
    ) {
        List<Predicate> conditions = new ArrayList<>();

        if (orderId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderId"), orderId)));
        }

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
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
            Root<OrderReturn> itemRoot) {
        List<Order> orderReturnList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("orderId")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("orderId")));
        }
        if (sortColumn.equalsIgnoreCase("clientId")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderReturnList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return orderReturnList;
    }

    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderReturn> itemRoot) {
        List<Order> orderReturnList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("orderId")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("orderId")));
        }
        if (sortColumn.equalsIgnoreCase("clientId")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderReturnList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return orderReturnList;
    }

    private long getOrderCount(
            Long orderId,
            Long clientId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderReturn> itemRoot = criteriaQuery.from(OrderReturn.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(
                orderId,
                clientId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                status,
                criteriaBuilder,
                itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
