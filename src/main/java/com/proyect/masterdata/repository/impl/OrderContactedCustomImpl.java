package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.OrderContacted;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.repository.OrderContactedRepositoryCustom;
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

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
@Repository
public class OrderContactedCustomImpl implements OrderContactedRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<OrderContacted> searchForContactedOrder(
            UUID clientId,
            Long orderNumber,
            Boolean contacted,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<OrderContacted> criteriaQuery = criteriaBuilder.createQuery(OrderContacted.class);
        Root<OrderContacted> itemRoot = criteriaQuery.from(OrderContacted.class);
        Join<OrderContacted, Ordering> orderContactedOrderingJoin = itemRoot.join("ordering");
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicate(
                orderNumber,
                contacted,
                clientId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                orderContactedOrderingJoin
        );
        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {
            List<Order> orderContactedList = new ArrayList<>();
            if (sort.equalsIgnoreCase("ASC")) {
                orderContactedList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }
            if (sort.equalsIgnoreCase("DESC")) {
                orderContactedList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(orderContactedList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }
        TypedQuery<OrderContacted> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderContactedCount(
                orderNumber,
                contacted,
                clientId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicate(
            Long orderNumber,
            Boolean contacted,
            UUID clientId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<OrderContacted> itemRoot,
            Join<OrderContacted,Ordering> orderContactedOrderingJoin
    ){
        List<Predicate> conditions = new ArrayList<>();

        if(clientId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"),clientId)));
        }

        if (orderNumber != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(
                                    orderContactedOrderingJoin.get("orderNumber"), orderNumber)));
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

        if (contacted) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("contacted"))));
        }

        if (!contacted) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("contacted"))));
        }

        return conditions;
    }
    List<Order> listASC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderContacted> itemRoot) {
        List<Order> orderContactedList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderContactedList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderContactedList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderContactedList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderContactedList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        return orderContactedList;
    }
    List<Order> listDESC(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<OrderContacted> itemRoot) {
        List<Order> orderContactedList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            orderContactedList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            orderContactedList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            orderContactedList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            orderContactedList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }
        return orderContactedList;
    }
    private long getOrderContactedCount(
            Long orderNumber,
            Boolean contacted,
            UUID clientId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<OrderContacted> itemRoot = criteriaQuery.from(OrderContacted.class);
        Join<OrderContacted, Ordering> orderContactedOrderingJoin = itemRoot.join("ordering");

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                orderNumber,
                contacted,
                clientId,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                orderContactedOrderingJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
