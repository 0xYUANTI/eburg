#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#define TRACE

enum { MOVE=1, MEM=2, PLUS=3, NAME=4, CONST=6 };

#define STATE_TYPE void*
typedef struct tree {
	int op;
	struct tree *kids[2];
	STATE_TYPE state_label;
} *NODEPTR_TYPE;
#define OP_LABEL(p) ((p)->op)
#define LEFT_CHILD(p) ((p)->kids[0])
#define RIGHT_CHILD(p) ((p)->kids[1])
#define STATE_LABEL(p) ((p)->state_label)
#define PANIC printf

static void burm_trace(NODEPTR_TYPE p, int eruleno, int cost, int bestcost) {
#ifdef TRACE
	extern char *burm_string[];

	fprintf(stderr, "0x%p matched %s with cost %d vs. %d\n", p,
		burm_string[eruleno], cost, bestcost);
#endif
}
#include <limits.h>
#include <stdlib.h>
#ifndef STATE_TYPE
#define STATE_TYPE int
#endif
#ifndef ALLOC
#define ALLOC(n) malloc(n)
#endif
#ifndef burm_assert
#define burm_assert(x,y) if (!(x)) { y; abort(); }
#endif

#define burm_stm_NT 1
#define burm_loc_NT 2
#define burm_reg_NT 3
#define burm_con_NT 4
int burm_max_nt = 4;

char *burm_ntname[] = {
	0,
	"stm",
	"loc",
	"reg",
	"con",
	0
};

struct burm_state {
	int op;
	struct burm_state *left, *right;
	short cost[5];
	struct {
		unsigned burm_stm:1;
		unsigned burm_loc:2;
		unsigned burm_reg:3;
		unsigned burm_con:1;
	} rule;
};

static short burm_nts_0[] = { burm_loc_NT, burm_reg_NT, 0 };
static short burm_nts_1[] = { burm_con_NT, burm_reg_NT, 0 };
static short burm_nts_2[] = { burm_reg_NT, burm_reg_NT, 0 };
static short burm_nts_3[] = { burm_loc_NT, 0 };
static short burm_nts_4[] = { burm_con_NT, 0 };
static short burm_nts_5[] = { burm_reg_NT, 0 };
static short burm_nts_6[] = { 0 };

short *burm_nts[] = {
	0,	/* 0 */
	burm_nts_0,	/* 1 */
	burm_nts_1,	/* 2 */
	burm_nts_2,	/* 3 */
	burm_nts_0,	/* 4 */
	burm_nts_3,	/* 5 */
	burm_nts_4,	/* 6 */
	burm_nts_5,	/* 7 */
	burm_nts_6,	/* 8 */
	burm_nts_5,	/* 9 */
	burm_nts_6,	/* 10 */
};

char burm_arity[] = {
	0,	/* 0 */
	2,	/* 1=MOVE */
	1,	/* 2=MEM */
	2,	/* 3=PLUS */
	0,	/* 4=NAME */
	0,	/* 5 */
	0,	/* 6=CONST */
};

char *burm_opname[] = {
	/* 0 */	0,
	/* 1 */	"MOVE",
	/* 2 */	"MEM",
	/* 3 */	"PLUS",
	/* 4 */	"NAME",
	/* 5 */	0,
	/* 6 */	"CONST",
};

short burm_cost[][4] = {
	{ 0 },	/* 0 */
	{ 4 },	/* 1 = stm: MOVE(MEM(loc),reg) */
	{ 3 },	/* 2 = reg: PLUS(con,reg) */
	{ 2 },	/* 3 = reg: PLUS(reg,reg) */
	{ 4 },	/* 4 = reg: PLUS(MEM(loc),reg) */
	{ 4 },	/* 5 = reg: MEM(loc) */
	{ 2 },	/* 6 = reg: con */
	{ 0 },	/* 7 = loc: reg */
	{ 0 },	/* 8 = loc: NAME */
	{ 0 },	/* 9 = loc: PLUS(NAME,reg) */
	{ 0 },	/* 10 = con: CONST */
};

char *burm_string[] = {
	/* 0 */	0,
	/* 1 */	"stm: MOVE(MEM(loc),reg)",
	/* 2 */	"reg: PLUS(con,reg)",
	/* 3 */	"reg: PLUS(reg,reg)",
	/* 4 */	"reg: PLUS(MEM(loc),reg)",
	/* 5 */	"reg: MEM(loc)",
	/* 6 */	"reg: con",
	/* 7 */	"loc: reg",
	/* 8 */	"loc: NAME",
	/* 9 */	"loc: PLUS(NAME,reg)",
	/* 10 */	"con: CONST",
};

static short burm_decode_stm[] = {
	0,
	1,
};

static short burm_decode_loc[] = {
	0,
	7,
	8,
	9,
};

static short burm_decode_reg[] = {
	0,
	2,
	3,
	4,
	5,
	6,
};

static short burm_decode_con[] = {
	0,
	10,
};

int burm_rule(STATE_TYPE state, int goalnt) {
	burm_assert(goalnt >= 1 && goalnt <= 4, PANIC("Bad goal nonterminal %d in burm_rule\n", goalnt));
	if (!state)
		return 0;
	switch (goalnt) {
	case burm_stm_NT:	return burm_decode_stm[((struct burm_state *)state)->rule.burm_stm];
	case burm_loc_NT:	return burm_decode_loc[((struct burm_state *)state)->rule.burm_loc];
	case burm_reg_NT:	return burm_decode_reg[((struct burm_state *)state)->rule.burm_reg];
	case burm_con_NT:	return burm_decode_con[((struct burm_state *)state)->rule.burm_con];
	default:
		burm_assert(0, PANIC("Bad goal nonterminal %d in burm_rule\n", goalnt));
	}
	return 0;
}

static void burm_closure_reg(struct burm_state *, int);
static void burm_closure_con(struct burm_state *, int);

static void burm_closure_reg(struct burm_state *p, int c) {
	if (c + 0 < p->cost[burm_loc_NT]) {
		p->cost[burm_loc_NT] = c + 0;
		p->rule.burm_loc = 1;
	}
}

static void burm_closure_con(struct burm_state *p, int c) {
	if (c + 2 < p->cost[burm_reg_NT]) {
		p->cost[burm_reg_NT] = c + 2;
		p->rule.burm_reg = 5;
		burm_closure_reg(p, c + 2);
	}
}

STATE_TYPE burm_state(int op, STATE_TYPE left, STATE_TYPE right) {
	int c;
	struct burm_state *p, *l = (struct burm_state *)left,
		*r = (struct burm_state *)right;

	assert(sizeof (STATE_TYPE) >= sizeof (void *));
	if (burm_arity[op] > 0) {
		p = ALLOC(sizeof *p);
		burm_assert(p, PANIC("ALLOC returned NULL in burm_state\n"));
		p->op = op;
		p->left = l;
		p->right = r;
		p->rule.burm_stm = 0;
		p->cost[1] =
		p->cost[2] =
		p->cost[3] =
		p->cost[4] =
			32767;
	}
	switch (op) {
	case 1: /* MOVE */
		assert(l && r);
		if (	/* stm: MOVE(MEM(loc),reg) */
			l->op == 2 /* MEM */
		) {
			c = l->left->cost[burm_loc_NT] + r->cost[burm_reg_NT] + 4;
			if (c + 0 < p->cost[burm_stm_NT]) {
				p->cost[burm_stm_NT] = c + 0;
				p->rule.burm_stm = 1;
			}
		}
		break;
	case 2: /* MEM */
		assert(l);
		{	/* reg: MEM(loc) */
			c = l->cost[burm_loc_NT] + 4;
			if (c + 0 < p->cost[burm_reg_NT]) {
				p->cost[burm_reg_NT] = c + 0;
				p->rule.burm_reg = 4;
				burm_closure_reg(p, c + 0);
			}
		}
		break;
	case 3: /* PLUS */
		assert(l && r);
		if (	/* loc: PLUS(NAME,reg) */
			l->op == 4 /* NAME */
		) {
			c = r->cost[burm_reg_NT] + 0;
			if (c + 0 < p->cost[burm_loc_NT]) {
				p->cost[burm_loc_NT] = c + 0;
				p->rule.burm_loc = 3;
			}
		}
		if (	/* reg: PLUS(MEM(loc),reg) */
			l->op == 2 /* MEM */
		) {
			c = l->left->cost[burm_loc_NT] + r->cost[burm_reg_NT] + 4;
			if (c + 0 < p->cost[burm_reg_NT]) {
				p->cost[burm_reg_NT] = c + 0;
				p->rule.burm_reg = 3;
				burm_closure_reg(p, c + 0);
			}
		}
		{	/* reg: PLUS(reg,reg) */
			c = l->cost[burm_reg_NT] + r->cost[burm_reg_NT] + 2;
			if (c + 0 < p->cost[burm_reg_NT]) {
				p->cost[burm_reg_NT] = c + 0;
				p->rule.burm_reg = 2;
				burm_closure_reg(p, c + 0);
			}
		}
		{	/* reg: PLUS(con,reg) */
			c = l->cost[burm_con_NT] + r->cost[burm_reg_NT] + 3;
			if (c + 0 < p->cost[burm_reg_NT]) {
				p->cost[burm_reg_NT] = c + 0;
				p->rule.burm_reg = 1;
				burm_closure_reg(p, c + 0);
			}
		}
		break;
	case 4: /* NAME */
		{
			static struct burm_state z = { 4, 0, 0,
				{	0,
					32767,
					0,	/* loc: NAME */
					32767,
					32767,
				},{
					0,
					2,	/* loc: NAME */
					0,
					0,
				}
			};
			return (STATE_TYPE)&z;
		}
	case 6: /* CONST */
		{
			static struct burm_state z = { 6, 0, 0,
				{	0,
					32767,
					2,	/* loc: reg */
					2,	/* reg: con */
					0,	/* con: CONST */
				},{
					0,
					1,	/* loc: reg */
					5,	/* reg: con */
					1,	/* con: CONST */
				}
			};
			return (STATE_TYPE)&z;
		}
	default:
		burm_assert(0, PANIC("Bad operator %d in burm_state\n", op));
	}
	return (STATE_TYPE)p;
}

#ifdef STATE_LABEL
static void burm_label1(NODEPTR_TYPE p) {
	burm_assert(p, PANIC("NULL tree in burm_label\n"));
	switch (burm_arity[OP_LABEL(p)]) {
	case 0:
		STATE_LABEL(p) = burm_state(OP_LABEL(p), 0, 0);
		break;
	case 1:
		burm_label1(LEFT_CHILD(p));
		STATE_LABEL(p) = burm_state(OP_LABEL(p),
			STATE_LABEL(LEFT_CHILD(p)), 0);
		break;
	case 2:
		burm_label1(LEFT_CHILD(p));
		burm_label1(RIGHT_CHILD(p));
		STATE_LABEL(p) = burm_state(OP_LABEL(p),
			STATE_LABEL(LEFT_CHILD(p)),
			STATE_LABEL(RIGHT_CHILD(p)));
		break;
	}
}

STATE_TYPE burm_label(NODEPTR_TYPE p) {
	burm_label1(p);
	return ((struct burm_state *)STATE_LABEL(p))->rule.burm_stm ? STATE_LABEL(p) : 0;
}

NODEPTR_TYPE *burm_kids(NODEPTR_TYPE p, int eruleno, NODEPTR_TYPE kids[]) {
	burm_assert(p, PANIC("NULL tree in burm_kids\n"));
	burm_assert(kids, PANIC("NULL kids in burm_kids\n"));
	switch (eruleno) {
	case 4: /* reg: PLUS(MEM(loc),reg) */
	case 1: /* stm: MOVE(MEM(loc),reg) */
		kids[0] = LEFT_CHILD(LEFT_CHILD(p));
		kids[1] = RIGHT_CHILD(p);
		break;
	case 3: /* reg: PLUS(reg,reg) */
	case 2: /* reg: PLUS(con,reg) */
		kids[0] = LEFT_CHILD(p);
		kids[1] = RIGHT_CHILD(p);
		break;
	case 5: /* reg: MEM(loc) */
		kids[0] = LEFT_CHILD(p);
		break;
	case 7: /* loc: reg */
	case 6: /* reg: con */
		kids[0] = p;
		break;
	case 10: /* con: CONST */
	case 8: /* loc: NAME */
		break;
	case 9: /* loc: PLUS(NAME,reg) */
		kids[0] = RIGHT_CHILD(p);
		break;
	default:
		burm_assert(0, PANIC("Bad external rule number %d in burm_kids\n", eruleno));
	}
	return kids;
}

int burm_op_label(NODEPTR_TYPE p) {
	burm_assert(p, PANIC("NULL tree in burm_op_label\n"));
	return OP_LABEL(p);
}

STATE_TYPE burm_state_label(NODEPTR_TYPE p) {
	burm_assert(p, PANIC("NULL tree in burm_state_label\n"));
	return STATE_LABEL(p);
}

NODEPTR_TYPE burm_child(NODEPTR_TYPE p, int index) {
	burm_assert(p, PANIC("NULL tree in burm_child\n"));
	switch (index) {
	case 0:	return LEFT_CHILD(p);
	case 1:	return RIGHT_CHILD(p);
	}
	burm_assert(0, PANIC("Bad index %d in burm_child\n", index));
	return 0;
}

#endif
static void dumpCover(NODEPTR_TYPE p, int goalnt, int indent) {
#ifdef TRACE
	int eruleno = burm_rule(STATE_LABEL(p), goalnt);
	short *nts = burm_nts[eruleno];
	NODEPTR_TYPE kids[10];
	int i;

	for (i = 0; i < indent; i++)
		fprintf(stderr, " ");
	fprintf(stderr, "%s\n", burm_string[eruleno]);
	burm_kids(p, eruleno, kids);
	for (i = 0; nts[i]; i++)
		dumpCover(kids[i], nts[i], indent + 1);
#endif
}

static NODEPTR_TYPE tree(int op, NODEPTR_TYPE l, NODEPTR_TYPE r) {
	NODEPTR_TYPE p = malloc(sizeof *p);

	assert(p);
	p->op = op;
	p->kids[0] = l; p->kids[1] = r;
	return p;
}

main(void) {
	NODEPTR_TYPE p;

	p = tree(MOVE,
		tree(MEM, tree(NAME, 0, 0), 0),
		tree(PLUS,
			tree(MEM, tree(PLUS,
				tree(NAME, 0, 0),
				tree(MEM, tree(NAME, 0, 0), 0)), 0),
			tree(CONST, 0, 0) ) );
	burm_label(p);
	dumpCover(p, 1, 0);
	return 0;
}
